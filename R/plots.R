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
  
  frq <- object@Connections$SFT$frequence
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
plotCells=function(object, 
                   cell.keep,
                   show.connections=T,
                   pt.size=3, 
                   color="red", 
                   real=F,
                   lt.size=0.1){
  
  plot_image_file <- object@Image
  poly_connected <- object@Connections$poly_connected
  poly_connected <- poly_connected %>% dplyr::filter(cells %in% {{cell.keep}})
  
  
  image_raster <- object@Image_real %>% scales::rescale(c(0,1))
  #image_raster[!is.na(object@Image)]<-0.5
  image_raster <- grDevices::as.raster(x = image_raster)
  
  
  
  if(real==T){
    
    image_raster <- object@Image_real %>% scales::rescale(c(0,1))
    #image_raster[!is.na(object@Image)]<-0.5
    image_raster <- grDevices::as.raster(x = image_raster)
    
    img_info <-
      image_raster %>%
      magick::image_read() %>%
      magick::image_info()
    
    st_image <-
      image_raster %>%
      magick::image_read() %>% 
      magick::image_rotate(90) %>% 
      magick::image_flop() %>% 
      magick::image_flip() %>% 
      magick::image_negate()
    
    #graphics::image(object@Image)
    
    
  }else{
    
    image_raster <- object@Image
    image_raster[!is.na(object@Image)]<-0.5
    image_raster <- grDevices::as.raster(x = image_raster)
    
    img_info <-
      image_raster %>%
      magick::image_read() %>%
      magick::image_info()
    
    st_image <-
      image_raster %>%
      magick::image_read() %>% 
      magick::image_rotate(90) %>% 
      magick::image_flop() %>% 
      magick::image_flip() %>% 
      magick::image_negate()
    
    #graphics::image(object@Image)
    
    
  }
  
  
  p <- ggplot()
  
  
  if(real==T){
    p=p+ggplot2::annotation_raster(raster = st_image,
                                   xmin = 0, ymin = 0,
                                   xmax = 1,
                                   ymax = 1)
  }else{
    p=p+ggplot2::annotation_raster(raster = st_image,
                                   xmin = 0, ymin = 0,
                                   xmax = 1,
                                   ymax = 1)
  }
  
  
  
 
  
  if(show.connections==T){
    p=p+ggplot2::geom_segment(data=poly_connected, mapping=aes(x=x1, xend=x2, y=y1, yend=y2), size=lt.size)
  }
  
  p=p+ggplot2::geom_point(data=poly_connected, mapping=aes(x=x1, y=y1),color=color, size=pt.size)
  
  p=p+theme_void()

  return(p)
  
  
  
  
}



#' @title plotVectorfield
#' @author Dieter Henrik Heiland
#' @description plotVectorfield
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

plotVectorfield <- function(object, real=F,dist.spot=0.01){
  
  neighbour <- object@Connections$poly_connected
  
  #Re-define parameters
  
  NN.file <- data.frame(sample= "Test",
                        bc_origin=paste0("cell_", object@Connections$poly_connected$cells),
                        bc_destination=paste0("cell_", object@Connections$poly_connected$To),
                        xo=object@Connections$poly_connected$x1,
                        yo=object@Connections$poly_connected$y1,
                        xd=object@Connections$poly_connected$x2,    
                        yd=object@Connections$poly_connected$y2, 
                        distance=object@Connections$poly_connected$dist) %>% as_tibble()
  
  
  df <- data.frame(barcodes=NN.file$bc_origin %>% unique()) %>% 
    mutate(x=purrr::map(.x=barcodes, function(x){NN.file %>% filter(bc_origin==x) %>% head(., 1) %>% pull(xo)} )%>% unlist(),
           y=purrr::map(.x=barcodes, function(x){NN.file %>% filter(bc_origin==x) %>% head(., 1) %>% pull(yo)})%>% unlist(),
           feat=purrr::map(.x=barcodes, function(x){neighbour %>% mutate(cells=paste0("cell_", cells)) %>% filter(cells==x) %>% pull(To) %>% unique() %>% length() }) %>% unlist() %>% as.numeric()
    )
  
  
  parameter="feat"
  
  message(" Create Vectorfield ")
  
  VF <- NeuroPhysiologyLab::getVectorfields(df,NN.file,parameter,dist.spot=dist.spot) %>% dplyr::select(x,y,{{parameter}}, t.x, t.y) %>% rename("parameter":=!!sym(parameter))
  
  if(real==T){
    
    image_raster <- object@Image_real %>% scales::rescale(c(0,1))
    #image_raster[!is.na(object@Image)]<-0.5
    image_raster <- grDevices::as.raster(x = image_raster)
    
    img_info <-
      image_raster %>%
      magick::image_read() %>%
      magick::image_info()
    
    st_image <-
      image_raster %>%
      magick::image_read() %>% 
      magick::image_rotate(90) %>% 
      magick::image_flop() %>% 
      magick::image_flip() %>% 
      magick::image_negate()
    
    #graphics::image(object@Image)
    
    
  }else{
    
    image_raster <- object@Image
    image_raster[!is.na(object@Image)]<-0.5
    image_raster <- grDevices::as.raster(x = image_raster)
    
    img_info <-
      image_raster %>%
      magick::image_read() %>%
      magick::image_info()
    
    st_image <-
      image_raster %>%
      magick::image_read() %>% 
      magick::image_rotate(90) %>% 
      magick::image_flop() %>% 
      magick::image_flip() %>% 
      magick::image_negate()
    
    #graphics::image(object@Image)
    
    
  }
  
  
  
  ggplot(data=VF, aes(x,y))+
    ggplot2::annotation_raster(raster = st_image,
                               xmin = 0, ymin = 0,
                               xmax = 1,
                               ymax = 1)+
    
    geom_point(data=VF , mapping=aes(x,y), size=0.2, alpha=0.5)+
    theme_classic()+
    metR::geom_vector(aes(dx = t.x, dy = t.y)) +
    metR::scale_mag()
  
  
  
}

#' @title plotVectorfield
#' @author Dieter Henrik Heiland
#' @description plotVectorfield
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'

plotVectorStream <- function(object, 
                             pt.size=1, 
                             pt.alpha=0.5, 
                             size.arrow=1, 
                             alpha.arrow=0.5, 
                             real=F,
                             dist.spot=0.01,
                             L=40,
                             res=0.5,
                             grid=c(40,40),
                             skip=2){
  
  neighbour <- object@Connections$poly_connected
  
  #Re-define parameters
  
  NN.file <- data.frame(sample= "Test",
                        bc_origin=paste0("cell_", object@Connections$poly_connected$cells),
                        bc_destination=paste0("cell_", object@Connections$poly_connected$To),
                        xo=object@Connections$poly_connected$x1,
                        yo=object@Connections$poly_connected$y1,
                        xd=object@Connections$poly_connected$x2,    
                        yd=object@Connections$poly_connected$y2, 
                        distance=object@Connections$poly_connected$dist) %>% as_tibble()
  
  
  df <- data.frame(barcodes=NN.file$bc_origin %>% unique()) %>% 
    mutate(x=purrr::map(.x=barcodes, function(x){NN.file %>% filter(bc_origin==x) %>% head(., 1) %>% pull(xo)} )%>% unlist(),
           y=purrr::map(.x=barcodes, function(x){NN.file %>% filter(bc_origin==x) %>% head(., 1) %>% pull(yo)})%>% unlist(),
           feat=purrr::map(.x=barcodes, function(x){neighbour %>% mutate(cells=paste0("cell_", cells)) %>% filter(cells==x) %>% pull(To) %>% unique() %>% length() }) %>% unlist() %>% as.numeric()
    )
  
  
  parameter="feat"
  
  message(" Create Vectorfield ")
  
  VF <- NeuroPhysiologyLab::getVectorfields(df,NN.file,parameter,dist.spot=dist.spot) %>% dplyr::select(x,y,{{parameter}}, t.x, t.y) %>% rename("parameter":=!!sym(parameter))
  
  
  if(real==T){
    
    image_raster <- object@Image_real %>% scales::rescale(c(0,1))
    #image_raster[!is.na(object@Image)]<-0.5
    image_raster <- grDevices::as.raster(x = image_raster)
    
    img_info <-
      image_raster %>%
      magick::image_read() %>%
      magick::image_info()
    
    st_image <-
      image_raster %>%
      magick::image_read() %>% 
      magick::image_rotate(90) %>% 
      magick::image_flop() %>% 
      magick::image_flip() %>% 
      magick::image_negate()
    
    #graphics::image(object@Image)
    
    
  }else{
    
    image_raster <- object@Image
    image_raster[!is.na(object@Image)]<-0.5
    image_raster <- grDevices::as.raster(x = image_raster)
    
    img_info <-
      image_raster %>%
      magick::image_read() %>%
      magick::image_info()
    
    st_image <-
      image_raster %>%
      magick::image_read() %>% 
      magick::image_rotate(90) %>% 
      magick::image_flop() %>% 
      magick::image_flip() %>% 
      magick::image_negate()
    
    #graphics::image(object@Image)
    
    
  }
  
  
  

# Create Streams ----------------------------------------------------------

  drifter.split.sf = VF %>% 
    sf::st_as_sf(coords = c("x", "y"))
  
  
  drifter.grid = drifter.split.sf %>% 
    sf::st_make_grid(n = grid)%>%
    sf::st_sf()
  
  drifter.split.sf.se = drifter.split.sf%>% filter(parameter!=0)
  
  drifter.gridded = drifter.grid %>% 
    mutate(id = 1:n(), contained = lapply(sf::st_contains(sf::st_sf(geometry),drifter.split.sf.se),identity),
           obs = sapply(contained, length),
           u = sapply(contained, function(x) {median(drifter.split.sf.se[x,]$t.x, na.rm = TRUE)}),
           v = sapply(contained, function(x) {median(drifter.split.sf.se[x,]$t.y, na.rm = TRUE)})) 
  
  
  
  drifter.gridded = drifter.gridded %>% dplyr::select(obs, u, v) %>% na.omit()
  
  ## obtain the centroid coordinates from the grid as table
  coordinates = drifter.gridded %>% 
    sf::st_centroid() %>% 
    sf::st_coordinates() %>% 
    as_tibble() %>% 
    rename(x = X, y = Y)
  
  ## remove the geometry from the simple feature of gridded drifter dataset
  sf::st_geometry(drifter.gridded) = NULL
  
  ## stitch together the extracted coordinates and drifter information int a single table for SE monsoon season
  current.gridded.se = coordinates %>% 
    dplyr::bind_cols(drifter.gridded) %>% 
    dplyr::mutate(season = "SE")
  
  ## bind the gridded table for SE and NE
  ## Note that similar NE follow similar procedure, hence not shown in the post
  drifter.current.gridded = current.gridded.se %>% 
    dplyr::bind_rows(current.gridded.se)
  
  
  
  
  ## select grids for SE season only
  drf.se = drifter.current.gridded %>% filter(season == "SE")
  
  ## interpolate the U component
  u.se = oce::interpBarnes(x = drf.se$x, y = drf.se$y, z = drf.se$u, gamma=0.5)
  
  ## obtain dimension that determine the width (ncol) and length (nrow) for tranforming wide into long format table
  dimension = data.frame(lon = u.se$xg, u.se$zg) %>% dim()
  
  ## make a U component data table from interpolated matrix
  u.tb = data.frame(lon = u.se$xg, 
                    u.se$zg) %>% 
    gather(key = "lata", value = "u", 2:dimension[2]) %>% 
    mutate(lat = rep(u.se$yg, each = dimension[1])) %>% 
    dplyr::select(lon,lat, u) %>% as.tibble()
  
  ## interpolate the V component
  v.se = oce::interpBarnes(x = drf.se$x, 
                           y = drf.se$y, 
                           z = drf.se$v,
                           gamma=0.5)
  
  ## make the V component data table from interpolated matrix
  v.tb = data.frame(lon = v.se$xg, v.se$zg) %>% 
    gather(key = "lata", value = "v", 2:dimension[2]) %>% 
    mutate(lat = rep(v.se$yg, each = dimension[1])) %>% 
    dplyr::select(lon,lat, v) %>% 
    as.tibble()
  
  ## stitch now the V component intot the U data table and compute the velocity
  uv.se = u.tb %>% 
    bind_cols(v.tb %>% dplyr::select(v)) %>% 
    mutate(vel = sqrt(u^2+v^2))
  
  library(oce)
  
  if(VF$parameter %>% class()=="factor"){
    ggplot2::ggplot() +
      ggplot2::annotation_raster(raster = st_image,
                                 xmin = 0, ymin = 0,
                                 xmax = 1,
                                 ymax = 1)+
      ggplot2::geom_point(data=VF, mapping=aes(x,y, color=parameter), size=pt.size, alpha=pt.alpha)+
      metR::geom_streamline(data = uv.se, 
                            aes(x = lon, y = lat, dx = u, dy = v),
                            size=size.arrow,
                            alpha=alpha.arrow,
                            arrow.length = 0.5,
                            arrow.angle = 25,
                            arrow.type = "closed",
                            L = L, res =res,skip=skip,
                            lineend = "round")+
      theme_void()
  }else{
    ggplot() +
      ggplot2::annotation_raster(raster = st_image,
                                 xmin = 0, ymin = 0,
                                 xmax = 1,
                                 ymax = 1)+
      ggplot2::geom_point(data=VF, mapping=aes(x,y, color=parameter), size=pt.size, alpha=pt.alpha)+
      ggplot2::scale_color_viridis_c(guide = "none") +
      metR::geom_streamline(data = uv.se, 
                            aes(x = lon, y = lat, dx = u, dy = v),
                            alpha=alpha.arrow,
                            size=size.arrow,
                            arrow.length = 0.5,
                            arrow.angle = 25,
                            arrow.type = "closed",
                            L = L, res =res,skip=skip,
                            lineend = "round")+
      theme_void()
  }
  
  
  
}

