# ST 558: Multivariate Analytics
# Homework 5
# Nora Quick

library(mclust)
library(ca)
library(psych)

# Import archaeo csv
archaeo <- read.csv('ArchaeoData.csv')
head(archaeo)

# Problem 1
# --------------
# Part (a)
archaeo.loc.mds0 <- cmdscale(archaeo, k=2) #assiming q=2 == k=2 
archaeo.loc.mds0

plot(archaeo.loc.mds0, pch=16)
text(archaeo.loc.mds0, labels=row.names(archaeo), pos=1, cex=0.5)

# flip
archaeo.loc.mds <- -archaeo.loc.mds0

plot(archaeo.loc.mds, pch=16)
text(archaeo.loc.mds, row.names(archaeo), pos=1, cex=0.5)


# Import track data
track <- read.csv('TrackData.csv')
head(track)

# Problem 2
# ---------------
# Part (a)
track.distEuc <- dist(track[3:10])

track.loc.mds0 <- cmdscale(track.distEuc, k=2)
track.loc.mds0

# Problem 2
# ---------------
# Part (b)
track.distCorr <- as.dist(1-cor(t(track[,-1][,-1]))^2)

track.loc.mds1 <- cmdscale(track.distCorr, k=2)
track.loc.mds1

# Problem 2
# ---------------
# Part (c)
plot(track.loc.mds0, pch=16, main="MDS Euclidean")
text(track.loc.mds0, labels = track[,1], pos=1, cex=0.5)

plot(track.loc.mds1, pch=16, main="MDS Correlation")
text(track.loc.mds1, labels = track[,1], pos=1, cex=0.5)


# Import state Migration data
mig <- read.csv('StateMigrationData.csv', row.names = 1)
head(mig)

# Problem 3
# ---------------
# Part (a)
mig.ca <- ca(mig)
mig.ca

plot(mig.ca)












