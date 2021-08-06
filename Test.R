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


path="analysis-A2_8bit.mat"

path="analysis-D1_8bit.mat"

path="analysis-E1_8bit.mat"

### Pipeline NeroPhysologyLab


object <- NeuroPhysiologyLab::readFluoroSNNAP(path)
object <- NeuroPhysiologyLab::findConnections(object, pal=viridis::viridis(50))
#NeuroPhysiologyLab::plotConnections(object, lwd = 1)
object <- NeuroPhysiologyLab::runAdjacencyMatrix(object)
#pheatmap::pheatmap(object@Connections$Adj_mat)
object <- NeuroPhysiologyLab::runSFT(object, breaks = 70)
#NeuroPhysiologyLab::plotFreq(object)
NeuroPhysiologyLab::plotSFT(object)

object@Spikes


object <- NeuroPhysiologyLab::findHotspots(object, select.freq = 2)
NeuroPhysiologyLab::plotHotspots(object, factor = 100000000000, lwd=log(object@Connections$Hotspots$frq+0.1)*10)

NeuroPhysiologyLab::plotVectorStream(object, alpha.arrow=0.1, real = F, dist.spot=0.1)
NeuroPhysiologyLab::plotVectorfield(object)

files <- dir()
files <- files[1:9]
files <- files[10:18]
all.mat <- purrr::map(.x=files, .f=function(path){
  
  object <- NeuroPhysiologyLab::readFluoroSNNAP(path)
  object <- NeuroPhysiologyLab::findConnections(object, pal=viridis::viridis(50))
  object <- NeuroPhysiologyLab::runAdjacencyMatrix(object)
  object <- NeuroPhysiologyLab::runSFT(object, breaks = 50)
  print(NeuroPhysiologyLab::getSFT.R2(object))
  object <- NeuroPhysiologyLab::findHotspots(object, select.freq = 2)
  
  return(object)
})


NeuroPhysiologyLab::plotRaster(all.mat[[1]])
NeuroPhysiologyLab::plotRaster(all.mat[[4]])
NeuroPhysiologyLab::plotRaster(all.mat[[7]])

i=1
SFT.df <- purrr::map_df(.x=all.mat, .f=function(object){
  dat <- data.frame(R2=NeuroPhysiologyLab::getSFT.R2(object))
  rownames(dat) <-  files[i]
  i<<-i+1 
  return(dat)
  })



# compare SFT
SFT.df.mean <- 
  SFT.df %>% 
  mutate(group=c("a", "a", "a", "b", "b", "b", "c", "c", "c")) %>% 
  filter(group %in% c("a","b", "c")) %>% 
  group_by(group) %>% 
  summarise_all(mean)
  
SFT.df.plot <- 
  SFT.df %>% 
  mutate(subgroup=c("a", "a", "a", "b", "b", "b", "c", "c", "c")) %>% 
  filter(subgroup %in% c("a","b","c")) %>% 
  arrange(subgroup) %>% 
  mutate(x=jitter(c(1, 1, 1, 2, 2, 2, 3, 3, 3)))

ggplot(SFT.df.plot, aes(x=x, y=R2, color=subgroup))+
  geom_point(size=3)+theme_classic()
confuns::plot_statistics_interactive(SFT.df.plot)

NeuroPhysiologyLab::plotConnections(object, lwd = 5)

NeuroPhysiologyLab::plotSFT(all.mat[[5]])
NeuroPhysiologyLab::plotConnections(all.mat[[2]], lwd = 5)

object <- NeuroPhysiologyLab::findHotspots(all.mat[[2]], select.freq = 4)
NeuroPhysiologyLab::plotHotspots(object, factor = 100000000000)

object <- NeuroPhysiologyLab::findHotspots(all.mat[[6]], select.freq = 10)
NeuroPhysiologyLab::plotHotspots(object, factor = 100000000000)


i=1
sc.Feat.df <- purrr::map_df(.x=all.mat, .f=function(object){
  dat <- getFeatures(object) %>% 
    mutate(sample=files[i])
  i<<-i+1
  return(dat)
})

sc.Feat.df <- 
  sc.Feat.df %>% 
  filter(sample %in% c(SFT.df.plot %>% rownames()) ) %>% 
  mutate(group=case_when(
    sample %in% c(SFT.df.plot %>% filter(subgroup=="a") %>% rownames())~"a",
    sample %in% c(SFT.df.plot %>% filter(subgroup=="b") %>% rownames())~"b",
    sample %in% c(SFT.df.plot %>% filter(subgroup=="c") %>% rownames())~"c",
  )) %>% 
  arrange(desc(group))



#confuns::plot_statistics_interactive(sc.Feat.df)

ggplot(sc.Feat.df, aes(x=Oscillation, y=DeltaAmplitude, color=group))+
  geom_point(size=2)+theme_classic()

ggplot(sc.Feat.df, aes(x=RiseT , y=FallT, color=group))+
  geom_point(size=2)+theme_classic()



pca <- irlba::prcomp_irlba(sc.Feat.df[,1:5] %>% t())
sc.Feat.df$PCA1 <- pca$rotation[,1]
sc.Feat.df$PCA2 <- pca$rotation[,2]
sc.Feat.df$PCA3 <- pca$rotation[,3]

ggplot(sc.Feat.df, aes(x=PCA1, y=PCA2, color=group))+
  geom_point(size=2)+theme_classic()

confuns::plot_statistics_interactive(sc.Feat.df)










object <- NeuroPhysiologyLab::findCellFrequency(object, min.events =5)
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

ggplot(plot.df, aes(x=sd+offset, y=cells))+geom_line()+theme_classic()+xlim(1,30)+ylim(0,30)+
  geom_line(mapping=aes(x=sd+offset, mean), color="red")+
  geom_line(mapping=aes(x=sd+offset, nr.cells), color="darkgreen")


i=5
cells.df <- lapply(1:length(Peacmaker[[i]]), function(x){Peacmaker[[i]][[x]]$Cell }) %>% unlist()
plotCells(object, cell.keep = cells.df)



# Render Totorials
rmarkdown::render("~/Desktop/Neurophysiology.Rmd")







