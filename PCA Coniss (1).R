library(vegan)
library(rioja)
library(cluster)
setwd("C:/Users/DELL/Dropbox/My PC (JERRYOLATOYAN)/Desktop/PCA analysis")
data<-read.csv("PCA new - Copy.csv", row.names=1)
#transform data (square root transformation)
data.t<-sqrt(data)

#run pca
pca<-rda(data.t,scale=FALSE)

#percent explained by each component
evals<-eigenvals(pca)
evals/sum(evals)
plot(pca)

#pc scores for sample and species
summary(pca)

#add arrows to biplot
plot(pca, type="none", xlim=c(-3,3), ylim=c(-2,1.5))
sp<-scores(pca,scaling=3, display="species")
arrows(0,0,sp[,1],sp[,2], length=0.1, col=2)
text(pca, display="species", cex=0.5)
points(pca, display="sites")
text(pl, display="sites", cex=0.5)

#CONISS
data.t<-sqrt(data)
data.dist<-vegdist(data.t,method="bray")
data.clu<-chclust(data.dist, method="coniss")
plot(data.clu,labels=NULL, hang=0.1, axes=TRUE, xvar=1:(length(data.clu$height)+1), xlim=NULL, ylim=NULL, x.rev=FALSE, horiz=FALSE)

#check number of groups
library(cluster)
sil<-silhouette(cutree(data.clu,2),data.dist)
plot(sil)

#final plot
plot(data.clu,labels=NULL, hang=0.1, axes=TRUE, xvar=1:(length(data.clu$height)+1), xlim=NULL, ylim=NULL, x.rev=FALSE, horiz=FALSE)

#for PCA
datagroups<-cutree(data.clu,k=2)

#colour
library(dendroextras)
par(mfrow=c(2,1))
par(mar=c(3,3,1,0))
dend<-colour_clusters(data.clu,k=2,col=1:2,groupLabels=TRUE)
plot(dend)

#how to colour the dots according to cluster analysis
#points
data.dist<-vegdist(data.t,method="bray")
data.clu<-chclust(data.dist, method="coniss")
datagroups<-cutree(data.clu,k=4)
par(mar=c(4.5,6,2,6))
plot(pca, type="none")
sp<-scores(pca, display="species")
arrows(0,0,sp[,1],sp[,2], length=0.1, col=2)
points(pca, display="sites", pch=19, col=datagroups)

#text
data.dist<-vegdist(data.t,method="bray")
data.clu<-chclust(data.dist, method="coniss")
datagroups<-cutree(data.clu,k=4)
par(mar=c(4.5,6,2,6))
plot(pca, type="none")
sp<-scores(pca, display="species")
text(pca, display="species", cex=0.5)
text(pca, display="sites", cex=0.5, col=datagroups)


