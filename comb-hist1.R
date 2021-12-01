library(oro.nifti)
library(neurobase)
library(tidyverse)
library(ggplot2)
library(imager)
library(caret)
library(reshape)

file.true <- "atlases/hammers-seg95/a1.nii.gz"
file.estimate <- "atlases/mgc2hammers-seg138/a1.nii.gz"

#Read manually segmented images
labels.true <- readnii(file.true) 
#Read automatically segmented images
labels.estimate <- readnii(file.estimate)

labels.true.list <- labels.true[ , , ] %>% as.vector
labels.estimate.list <- labels.estimate[ , , ] %>% as.vector

#Put voxel index and labels of each voxel in a data frame
df <- data.frame(Voxel = 1:length(labels.true), Reference = labels.true.list,Prediction = labels.estimate.list)
#Remove all voxels with only correctly overlaping background
df.short <- df[rowSums(df[,-1])>0,]

#the label thing thing
df.short$comb.index <- df.short$Reference*max(df.short$Prediction) + max(df.short$Prediction)

#plot histogram - Doesn't say much... Divide in each label 
ggplot(data=df.short, aes(comb.index)) + 
  geom_histogram(bins=max(df.short$comb.index))

################### Ground truth label 1, plot all predicted labels in histogram

labels.18 <- df.short[df.short$Reference == 18,,]

ggplot(data=labels.18, aes(Prediction)) + 
  geom_histogram(bins=max(lables.1$Prediction))

#Remove all correct classifications (47) and plot
labels.18.missclassifications <- labels.18[labels.18$Prediction != 39,,]
ggplot(data=labels.18.missclassifications, aes(Prediction)) + 
  geom_histogram(bins=max(labels.18.missclassifications$Prediction))
####################

#################### Classification matrix
#Why does it become a list..?
cm <- matrix(0,nrow = max(df.short$Reference)+1, ncol = max(df.short$Prediction)+1) %>% as.matrix

#For each reference label count predicted labels ad add to a matrix
for (ref in 1:max(df.short$Reference)) {
  label.pred <- df.short[df.short$Reference == ref,,]
  label.pred.count <- label.pred %>% count(Prediction) %>% as.matrix
  for (i in 1:length(label.pred.count[,1])) {
    pred <- label.pred.count[i,1]
    cm[ref, pred] <- label.pred.count[i,2]
  }
}

#####Sclae the thing! Can't actually see all misclassifications....
cm.scaled <- sweep(cm, 1, rowSums(cm), FUN="/") # apply(1, scale)
cm.scaled[is.nan(cm.scaled)] <- 0 #Probably not necessary... From problem with col scale
cm.scaled.df <- cm.scaled %>% melt

######Plot Plot Plot Plot scaled
ggplot(data=cm.scaled.df, aes(X2, X1, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "springgreen4") +
  coord_fixed()+
  xlab("Prediction") +
  ylab("Reference") +
  guides(fill = guide_colourbar(title = "Scaled count"))

#####Log insead!!!
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