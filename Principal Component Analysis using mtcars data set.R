### Load Data Set ####

data(mtcars)
head(mtcars)

## Creating New Dataframe ##

data_cars <- mtcars[,c (1:7,10,11)]
head(data_cars)

## Scatter Plot for Multicoliniarity ###
library("ggplot2")
library("GGally")
ggpairs(data_cars)

PCA <- prcomp(data_cars, center = TRUE, scale. = TRUE)
summary(PCA)
attributes(PCA)
print(PCA)
vr1 <- round(PCA$sdev[1]^2/sum(PCA$sdev^2)*100,2)
vr2 <- round(PCA$sdev[2]^2/sum(PCA$sdev^2)*100,2)
vr3 <- round(PCA$sdev[3]^2/sum(PCA$sdev^2)*100,2)
vr4 <- round(PCA$sdev[4]^2/sum(PCA$sdev^2)*100,2)

## Plotting ####
plot(PCA)
screeplot(x=PCA, type = "line", main = " Scree Plot")

## PCA Plotting ###
library(factoextra)
library(ggplot2)
library(ggfortify)
fviz_eig(PCA)
biplot(PCA, cex=0.5)
autoplot(PCA, scale = 0)

##########
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
autoplot(PCA,data = mtcars, color = "am",
         loadings = TRUE, loadings.colour = "blue",
         loadings.label= TRUE,
         loadings.label.size =3)
fviz_pca_ind(PCA,
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)
fviz_pca_var(PCA,
             col.var = "contrib",
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)
fviz_pca_biplot(PCA,
                repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")
fviz_pca_ind(PCA,
             col.ind = mtcars$am,
             palette = c("red","blue"),
             addEllipses = TRUE,
             legend.title="am",
             xlab=paste("PC1(",vr1,"%)",sep = ""),
             ylab=paste("PC2(",vr2,"%)",sep = ""),
             repel = TRUE)
