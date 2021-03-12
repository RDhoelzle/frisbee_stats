##Import libraries
library(vegan)
library(sciplot)
library(multcomp)
library(lsmeans)

##Define functions
#axis.percent 
#Returns the percentage varation on ordination axes
axis.percent <- function(ordination){
  round((100*eigenvals(ordination)[1:2]/ordination$tot.chi[[1]]),digits=2)
}

#custom.plot.pca
#Plots a PCA ordination
custom.plot.pca <- function(ord,title,plottype,scaling.val){
  plot(ord, type=plottype, scaling=scaling.val, 
       xlab=paste("PC1 (",axis.percent(
         ord)[[1]],"%)",sep=""),
       ylab=paste("PC2 (",axis.percent(
         ord)[[2]],"%)",sep=""),
       main=title)
  points(ord, dis='sp',pch=21,cex=3,scaling=3)
  points(ord,dis='sites',pch=4,col='red',cex=1,scaling=3)
}

##Prepare data
#Import
player_scores <- read.table('../data/player_totals.tsv', header=TRUE, sep='\t', row.names=1)

#Calc averages and subset
player_avg <- as.data.frame(t(player_scores[,-1]/player_scores[,1]))

#Calc unscaled PCA and plot
prin_comp <- rda(player_avg, scale = FALSE)
  custom.plot.pca(ord = prin_comp, title = "Principle Components", plottype = "n", scaling.val = 1)

#Extract player and metric principle components
players <- as.data.frame(prin_comp$CA$v)
metrics <- as.data.frame(prin_comp$CA$u)

#Write to tsv
write.table(players, file = "../results/player_pcs.tsv", row.names=TRUE, sep="\t")
write.table(metrics, file = "../results/metric_pcs.tsv", row.names=TRUE, sep="\t")
