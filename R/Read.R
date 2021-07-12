#' @title readFluoroSNNAP
#' @author Dieter Henrik Heiland
#' @description readFluoroSNNAP
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
#'


readFluoroSNNAP <- function(object, path){
  
  if(object %>% class()!="NeuoPhyhysiology") stop("object is not from class NeuoPhyhysiology ") else{ message("valid input object from class: NeuoPhyhysiology")}
  if(file.exists(path)==F) stop("Input mat file do not exist or the path is wrong")
  
  #Read in data
  Ca_Set=R.matlab::readMat(path)
  
  if(is.null(Ca_Set$processed.analysis)){
    Ca_Set$processed.analysis <- Ca_Set$data
  }
  
  
  #Import data into S4 object
  rownames(Ca_Set$processed.analysis)
  pos_L=which(rownames(Ca_Set$processed.analysis)=="L")
  
  
  # Frames and fps
  pos_image=which(rownames(Ca_Set$processed.analysis)=="fps")
  Framerate=as.numeric(Ca_Set[[1]][[pos_image]])
  object@Framerate=Framerate
  pos_image=which(rownames(Ca_Set$processed.analysis)=="Frames")
  Frames=as.numeric(Ca_Set[[1]][[pos_image]])
  object@Frames=Frames
  
  #modules=Ca_Set[[1]][[which(rownames(Ca_Set$processed.analysis)=="modules")]]
  #class(modules)
  #object@modules=modules
  
  # Import the image
  image=Ca_Set[[1]][[pos_L]]
  image[image==0]=NA
  object@Image=image
  
  
  pos_image=which(rownames(Ca_Set$processed.analysis)=="image")
  Test=as.matrix(Ca_Set[[1]][[pos_image]][,,2])
  dim(Test)
  object@Image_real=Test
  
  
  #scDisctiption
  OP=as.numeric(Ca_Set[[1]][[which(rownames(Ca_Set$processed.analysis)=="OP")]])
  OP[is.nan(OP)]=0 #Oscillation
  DF=as.numeric(Ca_Set[[1]][[which(rownames(Ca_Set$processed.analysis)=="DF")]])
  DF[is.nan(DF)]=0 #Delta Amplitude
  CV=as.numeric(Ca_Set[[1]][[which(rownames(Ca_Set$processed.analysis)=="CV")]])
  CV[is.nan(CV)]=0 #confidence of var
  
  rise.time=Ca_Set[[1]][[which(rownames(Ca_Set$processed.analysis)=="rise.time")]]
  fall.time=Ca_Set[[1]][[which(rownames(Ca_Set$processed.analysis)=="fall.time")]]
  
  
  
  #get df of means
  rise.time=sapply(1:length(rise.time), function(i) mean(rise.time[[i]][[1]]))
  rise.time[is.nan(rise.time)]=0
  
  fall.time=sapply(1:length(fall.time), function(i) mean(fall.time[[i]][[1]]))
  fall.time[is.nan(fall.time)]=0
  
  scDisctiption=data.frame(Oscillation=OP, DeltaAmplitude=DF, Variation=CV, RiseT=rise.time, FallT=fall.time )
  scDisctiption= scDisctiption %>% scale()
  
  object@scDisctiption=scDisctiption %>% as.data.frame()
  
  
  #dF.cell
  pos_dF.cell=which(rownames(Ca_Set$processed.analysis)=="dF.cell")
  Traces=Ca_Set[[1]][[pos_dF.cell]]
  object@Traces=Traces
  
  #Spipikes
  #Spikes.cell
  pos_Spikes.cell=which(rownames(Ca_Set$processed.analysis)=="Spikes.cell")
  Spikes=Ca_Set[[1]][[pos_Spikes.cell]]
  object@Spikes=Spikes
  
  
  Rasterplotfile=matrix(0, length(Spikes), ncol(Traces))
  for(i in 1:nrow(Rasterplotfile)){
    #print(i)
    spikes_i=round(as.numeric(Spikes[[i]][[1]]), digits = 0)
    Rasterplotfile[i, spikes_i]=1
  }
  object@Rasterplotfile=Rasterplotfile
  
  
  return(object)
  
  
}