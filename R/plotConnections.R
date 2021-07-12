#' @title test
#' @author Dieter Henrik Heiland
#' @description test
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#' 

plot_connections=function(image, 
                          poly_connected, 
                          plot_image_file=NA,
                          pal_image=brewer.pal(9,"Reds"),
                          alpha_image=0.8,  
                          alpha=1, lwd=NA, pal=viridis(50)){
  library(scales)
  if(length(plot_image_file)!=1){image(plot_image_file, col=alpha((pal_image), alpha_image), xaxt="n", yaxt="n")}else{image(image, col="lightgray", xaxt="n", yaxt="n")}
  if(!is.na(lwd[1])){lwd=lwd}else{lwd=1}
  points(x=poly_connected$x1,y=poly_connected$y1, pch=16, col=poly_connected$col, cex=0.5)
  points(x=poly_connected$x2,y=poly_connected$y3, pch=16, col=poly_connected$col, cex=0.5)
  for(z in 1:nrow(poly_connected)){polygon(x=c(poly_connected$x1[z],poly_connected$x2[z]),
                                           y=c(poly_connected$y1[z],poly_connected$y2[z]), 
                                           lwd=1, border=alpha(poly_connected$col[z], alpha), lty=1, lwd=lwd[z])} 
  
}