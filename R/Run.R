#' @title plotConnections
#' @author Dieter Henrik Heiland
#' @description plotConnections
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 

runAdjacencyMatrix <- function(object){
  
  #Empty Matrix
  Spikes <- object@Spikes
  Adj_mat=matrix(0, length(Spikes),  length(Spikes))
  Connections <- object@Connections
  #Add connections
  for(i in 1:nrow(Connections[[3]])){
    From=Connections[[3]][i, ]$From
    To=Connections[[3]][i, ]$To
    Adj_mat[From, To]=Adj_mat[From, To]+1
    Adj_mat[To, From]=Adj_mat[To, From]+1
  }
  
  Adj_mat=Adj_mat/max(Adj_mat)
  
  object@Connections$Adj_mat <- Adj_mat
  return(object)
}

#' @title plotConnections
#' @author Dieter Henrik Heiland
#' @description plotConnections
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#'

runSFT <- function(object, breaks=1000){
  
  #Isolate Events at similar start point
  EV=object@Connections$Events
  
  #More then one Activation 
  cells_multiple_p=unique(EV[duplicated(EV$Cells), ]$Cells)
  #Scale Free
  frq=data.frame(cell=1:max(EV$Cells), Freq=tabulate(EV$Cells))
  
  frq$connectivity=frq$Freq/max(frq$Freq)
  a=hist(frq$connectivity, breaks = breaks, plot=F)
  
  SFT <- list(frq, a)
  names(SFT) <- c("frequence", "histo")
  object@Connections$SFT <- SFT
  
  
  plot.df <- data.frame(x=log10(object@Connections$SFT$histo$counts), 
                        y=log10(object@Connections$SFT$histo$mids)) %>% 
    filter(is.finite(x))
  
  m=lm(plot.df$y~plot.df$x)
  Rsqrt=summary(m)$adj.r.squared
  print(paste0("R2 is: ", Rsqrt))
  object@Connections$SFT$R2 <-  Rsqrt
  
  
  
  return(object)
}


#' @title is.empty
#' @author Dieter Henrik Heiland
#' @description is.empty
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#'

is.empty <- function(x){if(base::length(x)==0){TRUE}else{FALSE}}




