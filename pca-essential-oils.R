setwd("the/working/directory")
dataset <- read.csv("dataset.csv", 
                    header = TRUE,    
                    sep = ";",
                    stringsAsFactors = FALSE)
head(dataset)

#prcomp returns an object with the PCA analysis
?prcomp
PCA <- prcomp(dataset[,3:123],center = T,scale. = F)

#calculate total variance
covariance <- cov(dataset[,3:123])
sum(diag(covariance))
eigenvalues <- PCA$sdev^2
sum(eigenvalues)

summary(PCA)
#plot the variance contributions 
screeplot(PCA,npcs = 7,type = "lines")
#select the first 3 PCs
PCAscores <- as.data.frame(PCA$x[, 1:3])

#labels and colors for the graphs
dataset$Genus <- as.factor(dataset$Genus)
Genus_colors <- rainbow(nlevels(dataset$Genus))[as.integer(dataset$Genus)]

# plotting the first 3 PCs
library(rgl)
plot3d(PCAscores[, 1:3], 
       col = Genus_colors,
       size = 3, 
       xlab = "PC1", 
       ylab = "PC2", 
       zlab = "PC3", 
       main = "3D PCA Scores")
rglwidget()

# plotting the first 2 PCs
plot(PCAscores[,-3], main="Scores", col = Genus_colors, pch = 15)
legend("topright", 
       legend = levels(dataset$Genus), 
       col = rainbow(nlevels(dataset$Genus)), 
       pch = 15, 
       title = "Genus")

