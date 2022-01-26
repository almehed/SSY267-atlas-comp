# MIT License
# 
# Copyright (c) 2022 Sara Almehed
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(oro.nifti)
library(neurobase)
library(tidyverse)
library(ggplot2)
library(reshape)
library(plotly)

################### Read all images and names ######################

#List of file paths, full.name = TRUE adds the path to the name 
#eg. "../atlases/mgc2hammers-seg138/a1.nii.gz"
files.true.HM <- list.files(path = "../atlases/hammers-seg95", 
                            pattern = ".nii.gz", 
                            full.names = TRUE)
files.estimate.MGC <- list.files(path = "../atlases/mgc2hammers-seg138", 
                                 pattern = ".nii.gz", 
                                 full.names = TRUE)

files.true.MGC <- list.files(path = "../atlases/mgc-seg138", 
                             pattern = ".nii.gz", 
                             full.names = TRUE)
files.estimate.HM <- list.files(path = "../atlases/hammers2mgc-seg95", 
                                pattern = ".nii.gz", 
                                full.names = TRUE)

#File paths for label names
file.HM.names <- "../atlases/hm-regionnames.tsv"
file.MGC.names <- "../atlases/miccaigc-regionnames.tsv"

#OBS ONLY CHANGE HERE!! Read all files in list of files -> list of nifti elements 
labels.true <- lapply(files.true.HM, readnii)
labels.estimate <-lapply(files.estimate.MGC, readnii)

#Read label names Change HM or MGC depending on true and estimate
names.true <- read_tsv(file.HM.names) %>% select(name)
names.estimate <- read_tsv(file.MGC.names) %>% select(name)


############# Reformate names and labels to formates to use #################

# Reformate name df as character vector
names.true.vec <- as.character(names.true$name)
names.estimate.vec <- as.character(names.estimate$name)

#Convert the nifti objects to lists/vectors and convert to single vector
labels.true.list <- lapply(labels.true, as.vector) %>% unlist
labels.estimate.list <- lapply(labels.estimate, as.vector) %>% unlist

#Create a data frame with voxels, reference (labels.true) 
#and prediction (labels.estimate)
df <- data.frame(Voxel = 1:length(labels.true.list), 
                 Reference = labels.true.list, 
                 Prediction = labels.estimate.list)

#Remove overlapping background
df.short <- df[rowSums(df[,-1])>0,]


################## Create histogram ####################

cm <- matrix(0,nrow = max(df.short$Reference)+1, 
             ncol = max(df.short$Prediction)+1) %>% as.matrix

#For each reference label count predicted labels and add to a matrix
for (ref in 0:max(df.short$Reference)) {
  label.pred <- df.short[df.short$Reference == ref,,]
  label.pred.count <- label.pred %>% count(Prediction) %>% as.matrix
  if (length(label.pred.count[,1]) > 0){
    for (i in 1:length(label.pred.count[,1])) {
      pred <- label.pred.count[i,1]
      cm[ref+1, pred+1] <- label.pred.count[i,2]
    }
  }
}


#log for scaling
cm.log <- log(cm+1)

############## Plot with plotly ##################

fig <- plot_ly(
  x = names.estimate.vec, #region names for estimate
  y = names.true.vec, #region names for true
  z = cm.log, type = "heatmap",
  colors = "Purples"
) %>% layout(xaxis = list(title = list(text = 'Prediction',standoff = 40L),
                            tickfont = list(size = 15),
                            titlefont = list(size = 20)),
               yaxis = list(title = list(text = 'Reference',standoff = 40L),
                            tickfont = list(size = 15),
                            titlefont = list(size = 20))
               )

fig

################## Plot using ggplot #######################

#convert to df on long format
cm.log.df <- cm.log %>% melt

#Move back indexes to start at zero not 1
cm.log.df$X1 <- cm.log.df$X1-1
cm.log.df$X2 <- cm.log.df$X2-1


fig2 <- ggplot(data=cm.log.df, aes(X1, X2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "springgreen4") +
  coord_fixed() +
  guides(fill = guide_colourbar(title = "log(count)"))+
  scale_y_continuous(name="Prediction", 
                     limits=c(-1, 100),
                     breaks = scales::pretty_breaks(n = 15)) +
  scale_x_continuous(name="Reference", 
                     limits=c(-1, 210), 
                     breaks = scales::pretty_breaks(n = 15))+
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "gray", size = 0.2),
    panel.grid.minor = element_line(colour = "gray", size = 0.2),
    panel.ontop = TRUE
  )

fig2
