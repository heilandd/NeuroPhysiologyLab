#' @title plotImage
#' @author Dieter Henrik Heiland
#' @description test
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#' 
plotImage <- function(object){
  graphics::image(object@Image_real, col=viridis::inferno(50), axes=F)
  }


#' @title map2color
#' @author Dieter Henrik Heiland
#' @description map2color
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
map2color<-function(x,pal,limits=NULL){
  if(class(x)=="numeric"){
    if(is.null(limits)) limits=range(x)
    pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
  }else{
    print(x[!duplicated(x)])
    da=data.frame(Terms=x[!duplicated(x)], Nr=seq(1,length.out = length(x[!duplicated(x)])))
    da$col=grDevices::colorRampPalette(pal)(nrow(da))[da[,2]]
    daf=data.frame(x=x, col=1)
    for(i in 1:length(x)){
      daf[i, ]$col=da[da$Terms==daf[i, "x"], ]$col
      
    }
    
    return(list(daf$col, da))
  }
  
}

#' @title plotTraces
#' @author Dieter Henrik Heiland
#' @description plotTraces
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
plotTraces=function(dat, TOP=nrow(dat), scale=5, lwd=1){
  
  dat=t(dat)
  rownames(dat)=1:nrow(dat)
  
  select=sapply(1:ncol(dat), function(i) max(dat[,i], na.rm=T))
  min_s=min(tail((select[order(na.omit(select), decreasing = F)]),TOP), na.rm=T)
  dat_top=dat[,which(select>min_s)]  
  
  
  plot(NA, xlim=c(-5,nrow(dat_top)+(ncol(dat_top)*3)), ylim = c(-2,TOP), axes = F, xlab="", ylab="")
  for(i in 1:ncol(dat_top)){
    points(y=(dat_top[,i]/(max(dat_top, na.rm = T)/scale))+i,x=as.numeric(rownames(dat_top))+i*2.5, type="l", lwd=lwd)
  }
  arrows(-3,0,nrow(dat)/10,0, length=0.1)
  arrows(-3,0,-3,TOP/10, length=0.1)
  
}



#' @title plotRaster
#' @author Dieter Henrik Heiland
#' @description plotTraces
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
plotRaster=function(object){
  cell=1:nrow(object@Rasterplotfile)
  plot(x=which(object@Rasterplotfile[cell,]==1, arr.ind=TRUE)[, 2], 
       y=which(object@Rasterplotfile[cell,]==1, arr.ind=TRUE)[, 1], xlab="Time",ylab="Cells",pch="|", col="black", yaxt="n", bty="o", cex=0.3)
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
#'

plotConnections=function(object, 
                          plot_image_file=NA,
                          pal_image=brewer.pal(9,"Reds"),
                          alpha_image=0.8,  
                          alpha=1, 
                          lwd=log(object@Connections$poly_connected$Time+1)/100, 
                          pal=viridis::viridis(50)){
  
  image <- object@Image
  poly_connected <- object@Connections$poly_connected
  if(length(lwd)==1){lwd=rep(lwd, object@Connections$poly_connected %>% nrow())}


  graphics::image(image, col="lightgray", xaxt="n", yaxt="n")
  
  if(!is.na(lwd[1])){lwd=lwd}else{lwd=1}
  
  graphics::points(x=poly_connected$x1,y=poly_connected$y1, pch=16, col=poly_connected$col, cex=0.5)
  graphics::points(x=poly_connected$x2,y=poly_connected$y3, pch=16, col=poly_connected$col, cex=0.5)
  for(z in 1:nrow(poly_connected)){polygon(x=c(poly_connected$x1[z],poly_connected$x2[z]),
                                           y=c(poly_connected$y1[z],poly_connected$y2[z]),border=alpha(poly_connected$col[z], alpha), lty=1, lwd=lwd[z])} 
  
}


#' @title plotSFT
#' @author Dieter Henrik Heiland
#' @description plotSFT
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

plotSFT <- function(object){
  
  plot.df <- data.frame(x=log10(object@Connections$SFT$histo$counts), 
                        y=log10(object@Connections$SFT$histo$mids)) %>% 
    filter(is.finite(x))
  
  m=lm(plot.df$y~plot.df$x)
  Rsqrt=summary(m)$adj.r.squared
  print(paste0("R2 is: ", Rsqrt))
  object@Connections$SFT$R2 <<- Rsqrt
  
  ggplot2::ggplot(plot.df, aes(x,y))+
    ggplot2::geom_point()+
    ggplot2::theme_classic()+
    ggplot2::geom_smooth(method = "lm", se=F, color="red", linetype = "dashed")+
    ggplot2::xlab("Log10(Connectivity)")+
    ggplot2::ylab("Log10(Frequency)")
  
  
}

#' @title plotFreq
#' @author Dieter Henrik Heiland
#' @description plotFreq
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

plotFreq <- function(object){
  
  freq <- object@Connections$SFT$frequence
  plot.df <- data.frame(x=1:nrow(frq), 
                        y=frq[order(frq$Freq, decreasing = T), ]$Freq)
  
  ggplot2::ggplot(plot.df, aes(x,y))+
    ggplot2::theme_classic()+
    ggplot2::geom_smooth(se=F, color="black")+
    ggplot2::xlab("Cells")+
    ggplot2::ylab("Frequency of Connections")

  
  
}


#' @title plotHotspots
#' @author Dieter Henrik Heiland
#' @description plotHotspots
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
plotHotspots=function(object, 
                         plot_image_file=NA,
                         pal_image=brewer.pal(9,"Reds"),
                         alpha_image=0.8,  
                         alpha=0.2,
                         factor=10^7,
                         lwd=log(object@Connections$Hotspots$frq+0.1)*2, 
                         pal=colorRampPalette(c("lightgray", "darkgreen", "darkred"))(3)){
  
  
  Color_Image_subset=function(image_select=object@Image, factor=1000000000, cells_to_mark=list(Hotspots$cells, Hotspots$To)){
    val=factor/length(cells_to_mark)
    for(i in 1:length(cells_to_mark)){
      image_select[image_select %in% unique(cells_to_mark[[i]])]=val*i
    }
    
    #image(image_select, col=rev(colorRampPalette(col)(2)))
    return(image_select)
  }
  
  
  Hotspots <- object@Connections$Hotspots
  poly_connected <- object@Connections$Hotspots
  if(length(lwd)==1){lwd=rep(lwd, object@Connections$poly_connected %>% nrow())}
  plot_image_file <- Color_Image_subset(image_select=object@Image, factor=,cells_to_mark=list(Hotspots$cells, Hotspots$To))
  
  graphics::image(plot_image_file, col=alpha((grDevices::colorRampPalette(c("lightgray", "darkgreen", "darkred"))(3)), alpha_image), xaxt="n", yaxt="n")
  
  
  graphics::points(x=poly_connected$x1,y=poly_connected$y1, pch=16, col=poly_connected$col, cex=0.5)
  graphics::points(x=poly_connected$x2,y=poly_connected$y3, pch=16, col=poly_connected$col, cex=0.5)
  for(z in 1:nrow(poly_connected)){polygon(x=c(poly_connected$x1[z],poly_connected$x2[z]),
                                           y=c(poly_connected$y1[z],poly_connected$y2[z]),
                                           border=alpha(poly_connected$col[z], alpha), 
                                           lty=1, 
                                           lwd=lwd[z])} 
  
}


#' @title plotHotspots
#' @author Dieter Henrik Heiland
#' @description plotHotspots
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'
plotCells=function(object, cell.keep, show.connections=T){
  
  plot_image_file <- object@Image
  poly_connected <- object@Connections$poly_connected
  
  poly_connected <- poly_connected %>% dplyr::filter(cells %in% {{cell.keep}})
  

  graphics::image(plot_image_file, col="lightgray", xaxt="n", yaxt="n")
  
  
  graphics::points(x=poly_connected$x1,y=poly_connected$y1, pch=16, col=poly_connected$col, cex=0.5)
  graphics::points(x=poly_connected$x2,y=poly_connected$y3, pch=16, col=poly_connected$col, cex=0.5)
  
  if(show.connections==T){
    for(z in 1:nrow(poly_connected)){polygon(x=c(poly_connected$x1[z],poly_connected$x2[z]),
                                             y=c(poly_connected$y1[z],poly_connected$y2[z]),
                                             border=alpha(poly_connected$col[z], alpha), 
                                             lty=1, 
                                             lwd=lwd[z])} 
    
  }

  
}


