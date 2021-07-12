library(NeuroPhysiologyLab)x
library(tidyverse)

Data.character=list(ExpID="Ca_imimaging_test", 
                    Researcher="KJ", 
                    Bioinformatic="Dieter Henri Heiland",
                    Institute="MILO Laboratory",
                    Date_of_Analysis="04.02.2020",
                    Datatype="Ca-Imaging",
                    Species="Human")

path="/Users/HenrikHeiland/Desktop/Projekt_Metabolom/Bioinformatic\ Tests/NeuroPhysiology_Lab/analysis_test.mat"
path="#372_B1_crop2_processed_analysis.mat"
setwd("~/Downloads/GlioGly files/Calcium Imaging Data/#372")

setwd("~/Desktop/Projekt_Metabolom/Bioinformatic Tests/NeuroPhysiology_Lab")
path="analysis_test.mat"


setwd("~/Downloads/GlioGly files/Calcium Imaging Data/233 KO and WT cells/mat files")
path="analysis-A1_8bit_crop1.mat"


object <- NeuroPhysiologyLab::NeuoPhyhysiology(Data.character=Data.character)
object <- NeuroPhysiologyLab::readFluoroSNNAP(object, path)

object <- NeuroPhysiologyLab::findConnections(object, pal=viridis::viridis(50))

NeuroPhysiologyLab::plotConnections(object, lwd = 1)



object <- NeuroPhysiologyLab::runAdjacencyMatrix(object)
pheatmap::pheatmap(object@Connections$Adj_mat)

object <- NeuroPhysiologyLab::runSFT(object, breaks = 50)

NeuroPhysiologyLab::plotFreq(object)
NeuroPhysiologyLab::plotSFT(object)



object <- NeuroPhysiologyLab::findHotspots(object, select.freq = 10)
NeuroPhysiologyLab::plotHotspots(object, factor = 1000000000000000)


object <- NeuroPhysiologyLab::findCellFrequency(object, min.events =50)

object <- NeuroPhysiologyLab::FindPeacemaker(object, min.sd =1:1000)

Peacmaker <- rlist::list.clean(object@Connections$Peacmaker, fun = NeuroPhysiologyLab::is.empty)
offset <- length(object@Connections$Peacmaker)-length(Peacmaker)
offset

plot.df <- 
purrr::map_df(.x=1:length(Peacmaker), .f=function(i){
  
  mean=lapply(1:length(Peacmaker[[i]]), function(x){Peacmaker[[i]][[x]]$mean }) %>% unlist()
  sd=lapply(1:length(Peacmaker[[i]]), function(x){Peacmaker[[i]][[x]]$sd }) %>% unlist()
  
  data.frame(sd=i,
             nr.cells=length(Peacmaker[[i]]),
             cells=mean(sd),
             mean=mean(mean))
  
})

ggplot(plot.df, aes(x=sd+offset, y=cells))+geom_line()+theme_classic()+xlim(1,100)+ylim(0,100)+
  geom_line(mapping=aes(x=sd+offset, mean), color="red")+
  geom_line(mapping=aes(x=sd+offset, nr.cells), color="darkgreen")


i=4
cells.df <- lapply(1:length(Peacmaker[[i]]), function(x){Peacmaker[[i]][[x]]$Cell }) %>% unlist()
plotCells(object, cell.keep = cells.df)









