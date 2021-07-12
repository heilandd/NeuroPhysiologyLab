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