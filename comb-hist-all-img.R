library(oro.nifti)
library(neurobase)
library(tidyverse)
library(ggplot2)
library(imager)
library(caret)
library(reshape)


########################## Read 2 images and compare ok! #################################
file.true1 <- "atlases/hammers-seg95/a1.nii.gz"
file.estimate1 <- "atlases/mgc2hammers-seg138/a1.nii.gz"
file.true2 <- "atlases/hammers-seg95/a2.nii.gz"
file.estimate2 <- "atlases/mgc2hammers-seg138/a2.nii.gz"

#Read manually segmented images
labels.true1 <- readnii(file.true1) 
labels.true2 <- readnii(file.true2) 

#Read automatically segmented images
labels.estimate1 <- readnii(file.estimate1)
labels.estimate2 <- readnii(file.estimate2)

labels.true.list1 <- labels.true1[ , , ] %>% as.vector
labels.estimate.list1 <- labels.estimate1[ , , ] %>% as.vector

labels.true.list2 <- labels.true2[ , , ] %>% as.vector
labels.estimate.list2 <- labels.estimate2[ , , ] %>% as.vector

labels.true.list <- c(labels.true.list1,labels.true.list2)
labels.estimate.list <- c(labels.estimate.list1,labels.estimate.list2)

#Put voxel index and labels of each voxel in a data frame
df <- data.frame(Voxel = 1:(length(labels.true1)+length(labels.true2)), Reference = labels.true.list,Prediction = labels.estimate.list)
df.short <- df[rowSums(df[,-1])>0,]

#########################################################################################

################################## Read all images ######################################

#List of file paths, full.name = TRUE adds the path to the name eg. "atlases/mgc2hammers-seg138/a1.nii.gz"
files.true.HM <- list.files(path = "atlases/hammers-seg95", pattern = ".nii.gz", full.names = TRUE)
files.estimate.MGC <- list.files(path = "atlases/mgc2hammers-seg138", pattern = ".nii.gz", full.names = TRUE)

#Read all files in list of files -> list of nifti elements
labels.true.HM <- lapply(files.true.HM, readnii)
labels.estimate.MGC <-lapply(files.estimate.MGC, readnii)

#Convert the nifti objects to lists/vectors
labels.true.HM.list <- lapply(labels.true.HM, as.vector)
labels.estimate.MGC.list <- lapply(labels.estimate.MGC, as.vector)

#Convert lists to single vector
labels.true.HM.vec <- unlist(labels.true.HM.list)
labels.estimate.MGC.vec <- unlist(labels.estimate.MGC.list)

#Create a data frame with voxels, reference (labels.true) and prediction (labels.estimate)
df <- data.frame(Voxel = 1:length(labels.true.HM.vec), Reference = labels.true.HM.vec, Prediction = labels.estimate.MGC.vec)
df.short <- df[rowSums(df[,-1])>0,]


cm <- matrix(0,nrow = max(df.short$Reference)+1, ncol = max(df.short$Prediction)+1) %>% as.matrix

#For each reference label count predicted labels ad add to a matrix
for (ref in 0:max(df.short$Reference)) {
  label.pred <- df.short[df.short$Reference == ref,,]
  label.pred.count <- label.pred %>% count(Prediction) %>% as.matrix
  for (i in 1:length(label.pred.count[,1])) {
    pred <- label.pred.count[i,1]
    cm[ref+1, pred+1] <- label.pred.count[i,2]
  }
}

cm.log <- log(cm+1)
cm.log.df <- cm.log %>% melt


######Plot Plot Plot Plot log
ggplot(data=cm.log.df, aes(X2, X1, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "springgreen4") +
  coord_fixed() +
  guides(fill = guide_colourbar(title = "log(count)"))+
  scale_x_continuous(name="Prediction", limits=c(0, 210)) +
  scale_y_continuous(name="Reference", limits=c(0, 100))+
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "gray"),
    panel.grid.minor = element_line(colour = "gray"),
    panel.ontop = TRUE
  )

