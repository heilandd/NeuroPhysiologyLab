
# Create package
devtools::create('NeuroPhysiologyLab')

library(devtools)
setwd("~/Desktop/Projekt_Metabolom/Bioinformatic Tests/NeuroPhysiologyLab")

# Updata namespace
document() #<- Namespace usw
load_all() # <- library
library(NeuroPhysiologyLab)


