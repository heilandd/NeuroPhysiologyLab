#' @title getPosition
#' @author Dieter Henrik Heiland
#' @description getPosition
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
#'
getPosition=function(cell, image){
  cell_mat=image
  cell_mat[cell_mat!=cell]=NA
  pos=which(!is.na(cell_mat), arr.ind=TRUE)
  dim=dim(cell_mat)
  pos[,1]=pos[,1]/dim[1]
  pos[,2]=pos[,2]/dim[2]
  pos_cell_From=c(x=median(pos[,1]), y=median(pos[,2]))
  return(pos_cell_From)
}

#' @title getDistance
#' @author Dieter Henrik Heiland
#' @description getDistance
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

getDistance=function(p1x,p1y, p2x,p2y){
  p1x=as.numeric(p1x)
  p1y=as.numeric(p1y)
  p2x=as.numeric(p2x)
  p2y=as.numeric(p2y)
  dist_out=sqrt( (abs(p2x-p1x)^2) + (abs(p2y-p1y)^2)    )
  return(dist_out)
}


#' @title getVectorfields
#' @author Dieter Henrik Heiland
#' @description getVectorfields
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

getVectorfields<-function(df,NN.file,parameter, Ram.free=50000, workers=8, dist.spot=0.01){
  

# Set up multiocore -------------------------------------------------------

  
  base::options(future.fork.enable = TRUE)
  future::plan("multiprocess", workers = workers)
  future::supportsMulticore()
  base::options(future.globals.maxSize = Ram.free * 1024^2)
  base::message("... Run multicore ... ")


# Create Vectorfields -----------------------------------------------------


  VF <- furrr::future_map(.x=1:nrow(df), .f=function(i){
    
    bc <- df[i, c("barcodes")]
    cc <- df[i, c("x", "y")]
    
    #NN <- NN.file %>% filter(bc_origin=={{bc}}) %>% pull(bc_destination)
    
    NN <- NN.file %>% 
      filter(xo < cc$x+dist.spot & xo > cc$x-dist.spot) %>% 
      filter(yo < cc$y+dist.spot & yo > cc$y-dist.spot) %>%
      pull(bc_destination)
    
    
    NN.df <- df %>% filter(barcodes %in% NN) %>% as.data.frame()
    
    
    
    # create vector
    V <- -c(
      as.numeric(cc) - c(NN.df$x[which.max(NN.df[,parameter])], NN.df$y[which.max(NN.df[,parameter])])
    )
    
    
    
    #V <- c(NN$x[which.max(NN$z)], NN$y[which.max(NN$z)]) * cor[i, c("z")]
    if(length(V)==0){
      out <- data.frame(barcodes=bc, t.x=0, t.y=0)
    }else{out <- data.frame(barcodes=bc, t.x=V[1], t.y=V[2])}
    
    
    return(out)
    
    
    
  }, .progress = T) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    left_join(., df, by="barcodes")
  
  VF[is.na(VF)] <- 0
  
  return(VF)
  
  
}



#' @title getSFT.R2
#' @author Dieter Henrik Heiland
#' @description getSFT.R2
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
getSFT.R2 <- function(object){return(object@Connections$SFT$R2)}


#' @title getSFT.R2
#' @author Dieter Henrik Heiland
#' @description getSFT.R2
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
getFeatures <- function(object){return(object@scDisctiption)}









