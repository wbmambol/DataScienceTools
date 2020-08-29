# DataScienceTools
New Data Science Student
Principle Component Analysis (PCA) + Visualization
#Load the data
load("~/Downloads/LeukError.RData")
colnames(leuk)
#Look at summarry of the matrix to see how big / small data points are.
summary(leuk[,4890:5000])
#Correlation between sample V and Type of leukimia
      #If a standard deviation is equal to zero the associated quantity is constant, 
      #so the notion of linear relationship between it and the other doesn't apply. 
      #Mathematically, in one of the standard deviations is zero the correlation involves division by zero, 
      #so it (the correlation) is undefined.
# Create empty data set for saving std devs
stds_obs = NULL
# Calcualte std dev for each obs and save to new stds_obs data set
luk = leuk[-5001]
cancer_labels=leuk[5001]
for (i in 1:5000) {
  stds_obs[i] = sd(luk[,i])
}
# Remove obs w/no std dev (i.e. constant values)
luk = luk[stds_obs != 0]

#If a variable has no variation it cannot have co-variation
cor.matrix=cor(luk) #problem


#Scale > 0-100 therefore use covariance PCA (AKA include scale) else don't include Scale
#If scores were 0-100, I don't need to standardize, in this case We ARE standardizing.
pca=prcomp(luk, center = TRUE, scale = TRUE )
summary(pca)
#Look into the PCA Object
str(pca) #Lists samples char 1-38

library(ggbiplot)

ggbiplot(pca)

ggbiplot(pca, labels=cancer_labels)

#add the response variable (diagnosis) to the plot and see if we can make better sense of it:
plot(pca$x[,1],pca$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")

ggbiplot(pca)

library("FactoMineR")
library("factoextra")
fviz_pca_ind(pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = leuk[,5001], #put your response var in here.
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Type of Leukemia:") +
  ggtitle("2D PCA-plot from 38 Samples") +
  theme(plot.title = element_text(hjust = 0.5))
