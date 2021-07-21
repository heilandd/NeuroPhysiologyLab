#' @title findConnections
#' @author Dieter Henrik Heiland
#' @description findConnections
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

findConnections=function(object,
                         time_shift=10,
                         distance_max=0.1, 
                         pal=viridis::viridis(50), 
                         parameter_to_col=c("dist", "Time")[1], 
                         cores=8,
                         Events_select=NULL){
  

# Get parameter from object -----------------------------------------------

  
  Spikes <- object@Spikes
  image <- object@Image
  

# Create Events -----------------------------------------------------------

  message("Step 1/4: Create Events")
  Events=as.data.frame(do.call(rbind,pbmcapply::pbmclapply(1:length(Spikes),mc.cores=cores, function(i){
    #print(i)
    Time=as.numeric(Spikes[[i]][[1]])
    
    if(length(Time)>=1){
      df_n=data.frame(Time=as.numeric(Spikes[[i]][[1]]),Cells=i)
      #print(df_n)
    } else(df_n=data.frame(Time=NA,Cells=i))
    
  })))
  

  Events=na.omit(Events)
  Events=Events[order(Events$Time, decreasing = F), ]
  
  if(!is.null(Events_select)){Events=Events[Events_select, ]}
  
  message(paste0("Number of Events were extracted: ", nrow(Events)))
  
  message("Step 2/4: Quantify Connections and measure Distance----Get Position")
  
  
  ## Get positions of each Event
  Events$x=NA
  Events$y=NA
  pos_forloop <- as.data.frame(do.call(rbind, pbmcapply::pbmclapply(1:nrow(Events),mc.cores=cores, function(z){pos=NeuroPhysiologyLab::getPosition(Events[z, "Cells"], image)})))
  Events$x=as.numeric(pos_forloop[,1])
  Events$y=as.numeric(pos_forloop[,2])
  
  
  
  
  message("Step 2/4: Quantify Connections and measure Distance----Get Connections")
  
  message(paste0("For Parallel Computation: ", cores, " Cores are selected"))
  Connections_unselected <- pbmcapply::pbmclapply(1:nrow(Events),mc.cores=cores, function(i){
    #print(i)
    #measure distance from first cell
    cell=Events[i, "Cells"]
    
    
    #Filter Data based on distance and Time
    collect=Events[i, "Time"]+time_shift
    Events_i=Events[i:max(which(Events$Time<collect)), ]
    
    
    
    #Get positions
    #pos_host=get_position(cell, image=sample@Image)
    #pos_receiver=data.frame(do.call(rbind, lapply(1:nrow(Events_i), function(z){get_position(Events_i[z, "Cells"], image=sample@Image)})))
    
    
    #Create a position patrix
    pos_mat=data.frame(p1x=as.numeric(Events_i[1,"x"]),
                       p1y=Events_i[1,"y"],
                       p2x=as.numeric(Events_i[1:nrow(Events_i),"x"]),
                       p2y=Events_i[1:nrow(Events_i),"y"])
    
    
    distance=NeuroPhysiologyLab::getDistance(p1x=pos_mat$p1x,p1y=pos_mat$p1y,p2x=pos_mat$p2x,p2y=pos_mat$p2y)
    
    
    Events_i$distance=distance
    
    Events_i=Events_i[which(Events_i$distance<distance_max), ]
    if(nrow(Events_i)==0){Events_i=NA}else{if(unique(c(Events_i$Cells %in% cell))!=1 | nrow(Events_i)==0){Events_i=NA}}
    
    
    
    return(Events_i)
    
    
    
    
  })
  
  
  message("Step 3/4: Optimize and Filter Connections")
  connections=as.data.frame(do.call(rbind, pbmcapply::pbmclapply(1:length(Connections_unselected),mc.cores=cores, function(i){
    #print(i)
    if(!is.na(Connections_unselected[[i]])){
      cells_con=unique(Connections_unselected[[i]]$Cells)
      if(length(cells_con)>1){
        #Create connection nodes
        
        connections_x=as.data.frame(do.call(rbind, lapply(2:ncol(Connections_unselected[[i]]), function(z){
          start_from=Connections_unselected[[i]]$Cells[1]
          nodes=Connections_unselected[[i]]$Cells[z]
          
          connections_x=data.frame(From=start_from, 
                                   To=nodes, col="black", 
                                   dist=Connections_unselected[[i]]$distance[z], 
                                   Time=Connections_unselected[[i]]$Time[z],
                                   Nr_of_con=i)
          
          
        })))
      }else{connections_x=data.frame(From=NA,  To=NA,     col=NA, dist=NA, Time=NA, Nr_of_con=NA)}
    }else{connections_x=data.frame(From=NA,  To=NA,     col=NA, dist=NA, Time=NA,Nr_of_con=NA)}
    
    #print(connections_x)
    
    return(connections_x)
  })))
  #Remove duplicates
  connections=na.omit(connections)

  
  message(paste0("Numbers of connections: ", nrow(connections)))
  connections$col=map2color(connections[,parameter_to_col], pal)
  message("Step 4/4: Create Plot File")
  #Start Plot Output
  connected=connections
  poly_connected=as.data.frame(do.call(rbind, pbmcapply::pbmclapply(1:nrow(connected),mc.cores=cores, function(i){
    
    #from
    #Calculate Mean position of the cell
    cell=connected[i,1]
    pos_cell_From=c(x=as.numeric(Events[Events$Cells==cell, "x"][1]), y=as.numeric(Events[Events$Cells==cell, "y"][1]))
    
    #To
    cell=connected[i,2]
    pos_cell_to=c(x=as.numeric(Events[Events$Cells==cell, "x"][1]), y=as.numeric(Events[Events$Cells==cell, "y"][1]))
    
    #x=c(pos_cell_From[1], pos_cell_to[1])
    #y=c(pos_cell_From[2], pos_cell_to[2])
    
    
    return(data.frame(x1=pos_cell_From[1],x2=pos_cell_to[1], y1=pos_cell_From[2], y2=pos_cell_to[2]))
    
  })))
  poly_connected$col=connected$col
  poly_connected$cells=connections$From
  poly_connected$To=connections$To
  poly_connected$dist=connections$dist
  poly_connected$Time=connections$Time
  poly_connected$Nr_of_con=connections$Nr_of_con
  out=list(Events, Connections_unselected, connections, poly_connected)
  names(out)=c("Events", "Connections_unselected", "connections", "poly_connected")
  
  message("Finish Pipeline")
  
  object@Connections <- out
  
  return(object)
  
}


#' @title findConnections
#' @author Dieter Henrik Heiland
#' @description findConnections
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

findHotspots <- function(object, 
                         select.freq=20,
                         cores=8){
  
  frq <- object@Connections$SFT$frequence
  
  #select cells with high freq
  cells_multiple_p <- frq[frq$Freq>select.freq, ]$cell
  
  #Remove cells which are not connected but have events
  con <- object@Connections$poly_connected
  con <- con[order(con$Time), ]
  
  cells_multiple_p <- intersect(cells_multiple_p, unique(con$cells))
  
  
  
  Hotspots=data.frame(do.call(rbind,pbmcapply::pbmclapply(1:length(cells_multiple_p), mc.cores = cores, function(z){
    
    #Select High connected cells
    pos_activation=which(con$cells==cells_multiple_p[z])
    
    
    #plot same host Cell
    
    dat_host=as.data.frame(do.call(rbind, lapply(1:length(pos_activation), function(i){
      #print(i)
      cells_select=con[con$Nr_of_con==con[pos_activation[i], ]$Nr_of_con, ]
      cells_select$rep=i
      
      #Remove duplets
      frq=data.frame(cell=1:max(unique(cells_select$To)), Freq=tabulate(cells_select$To))
      frq=frq[frq$Freq!=0, ]
      
      cells_select=cells_select[!duplicated(cells_select$To) , ]
      frq=frq[as.character(cells_select$To), ]
      cells_select$frq=frq$Freq
      
      
      return(cells_select)
      
      
    })))
    
    dat_host$col="black"
    
    return(dat_host)
  })))
  
  
  object@Connections$Hotspots <- Hotspots
  
  return(object)
  
  
  
  
}


#' @title findCellFrequency
#' @author Dieter Henrik Heiland
#' @description findCellFrequency
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
findCellFrequency <- function(object, min.events=5){
  
  
  # Get Spikes
  Spikes <- object@Spikes
  
  frequence <- purrr::map(.x=Spikes, .f=function(sp){
    Cell <- sp[[1]] %>% as.numeric()
    if(!length(Cell)<min.events){
    Cell.inter <- purrr::map_dbl(.x=2:length(Cell), .f=function(i){
        inter <- Cell[i]-Cell[i-1]
      })
     out <-  
      list(Cell.inter,
           mean(Cell.inter),
           median(Cell.inter),
           IQR(Cell.inter),
           sd(Cell.inter)
      )
      names(out) <- c("Intervall", "mean", "median", "IQR", "sd")
      
      return(out)
      
    }
    
    
  })
  
  
  object@Connections$Frequence <- frequence
  
  return(object)
  
  
  
}



#' @title FindPacemaker
#' @author Dieter Henrik Heiland
#' @description FindPacemaker
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

findPacemaker <- function(object, min.sd=15, verbose=T ){
  
  frequence <- object@Connections$Frequence
  
  if(length(min.sd)==1){
  
  
  selected.frequence <- map(.x=1:length(frequence), .f=function(i){
    ls <- frequence[[i]]
    if(is.null(ls)){if(verbose==T){message("Slot is empty")}}else{
      if(ls$sd<min.sd){ls$Cell <- i; return(ls)}
      }
    
  
  
})
  
  #Clean list
  
  selected.frequence <- rlist::list.clean(selected.frequence)
  
  #message
  message("With the curent parameters, ", length(selected.frequence), " Pacemaker cells were observed")
  
  
  object@Connections$Pacemaker <- selected.frequence
  }
  if(length(min.sd)>1){
    
  multi.freq <- pbmcapply::pbmclapply(1:length(min.sd), function(x){
      
      selected.frequence <- map(.x=1:length(frequence), .f=function(i){
        ls <- frequence[[i]]
        if(is.null(ls)){if(verbose==T){message("Slot is empty")}}else{
          if(ls$sd<x){ls$Cell <- i; return(ls)}
        }
        
        
        
      })
      
      #Clean list
      
      selected.frequence <- rlist::list.clean(selected.frequence)
      
      #message
      message("With the curent parameters, ", length(selected.frequence), " Pacemaker cells were observed")
      
      return(selected.frequence)
      
    })
  
  object@Connections$Pacemaker <- multi.freq
 
  }
  
  return(object)
  
  
  
}






