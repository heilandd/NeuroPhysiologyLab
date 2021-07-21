library(NeuroPhysiologyLab)
library(tidyverse)

path="/Users/HenrikHeiland/Desktop/Projekt_Metabolom/Bioinformatic\ Tests/NeuroPhysiology_Lab/analysis_test.mat"
path="#372_B1_crop2_processed_analysis.mat"
setwd("~/Downloads/GlioGly files/Calcium Imaging Data/#372")

setwd("~/Desktop/Projekt_Metabolom/Bioinformatic Tests/NeuroPhysiology_Lab")
path="analysis_test.mat"


setwd("~/Downloads/GlioGly files/Calcium Imaging Data/233 KO and WT cells/mat files")
path="analysis-A1_8bit_crop1.mat"

#WT
path="analysis-Glut_A2_8bit_crop1.mat"
path="analysis-Glut_A2_8bit_crop2.mat"

#KO
path="analysis-Glut_D3_8bit_crop1.mat"
path="analysis-Glut_D3_8bit_crop2.mat"

path="analysis-Glut_E2_8bit_crop1.mat"
path="analysis-Glut_E2_8bit_crop2.mat"

files=dir()

### Pipeline NeroPhysologyLab


object <- NeuroPhysiologyLab::readFluoroSNNAP(path)

object <- NeuroPhysiologyLab::findConnections(object, pal=viridis::viridis(50))
NeuroPhysiologyLab::plotConnections(object, lwd = 1)



object <- NeuroPhysiologyLab::runAdjacencyMatrix(object)
pheatmap::pheatmap(object@Connections$Adj_mat)

object <- NeuroPhysiologyLab::runSFT(object, breaks = 50)
NeuroPhysiologyLab::plotFreq(object)
NeuroPhysiologyLab::plotSFT(object)



object <- NeuroPhysiologyLab::findHotspots(object, select.freq = 2)
NeuroPhysiologyLab::plotHotspots(object, factor = 100000000000)

NeuroPhysiologyLab::plotVectorStream(object, alpha.arrow=0.1, real = F, dist.spot=0.1)
NeuroPhysiologyLab::plotVectorfield(object,dist.spot=0.001)



all.mat <- purrr::map(.x=files, .f=function(path){
  
  object <- NeuroPhysiologyLab::readFluoroSNNAP(path)
  object <- NeuroPhysiologyLab::findConnections(object, pal=viridis::viridis(50))
  object <- NeuroPhysiologyLab::runAdjacencyMatrix(object)
  object <- NeuroPhysiologyLab::runSFT(object, breaks = 50)
  NeuroPhysiologyLab::plotSFT(object)
  print(NeuroPhysiologyLab::getSFT.R2(object))
  object <- NeuroPhysiologyLab::findHotspots(object, select.freq = 2)
  
  return(object)
})



i=1
purrr::map_df(.x=all.mat, .f=function(object){
  dat <- data.frame(R2=NeuroPhysiologyLab::getSFT.R2(object))
  rownames(dat) <-  files[i]
  i<<-i+1 
  return(dat)
  })




object <- NeuroPhysiologyLab::findCellFrequency(object, min.events =10)
object <- NeuroPhysiologyLab::findPacemaker(object, min.sd =1:30)

Pacemaker <- rlist::list.clean(object@Connections$Pacemaker, fun = NeuroPhysiologyLab::is.empty)
offset <- length(object@Connections$Pacemaker)-length(Pacemaker)
offset

plot.df <- 
purrr::map_df(.x=1:length(Pacemaker), .f=function(i){
  
  mean=lapply(1:length(Pacemaker[[i]]), function(x){Pacemaker[[i]][[x]]$mean }) %>% unlist()
  sd=lapply(1:length(Pacemaker[[i]]), function(x){Pacemaker[[i]][[x]]$sd }) %>% unlist()
  
  data.frame(sd=i,
             nr.cells=length(Pacemaker[[i]]),
             cells=mean(sd),
             mean=mean(mean))
  
})

ggplot(plot.df, aes(x=sd+offset, y=cells))+geom_line()+theme_classic()+xlim(1,300)+ylim(0,300)+
  geom_line(mapping=aes(x=sd+offset, mean), color="red")+
  geom_line(mapping=aes(x=sd+offset, nr.cells), color="darkgreen")


i=5
cells.df <- lapply(1:length(Peacmaker[[i]]), function(x){Peacmaker[[i]][[x]]$Cell }) %>% unlist()
plotCells(object, cell.keep = cells.df)











