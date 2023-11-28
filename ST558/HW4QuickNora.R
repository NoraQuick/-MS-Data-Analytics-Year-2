# ST 558: Multivariate Analytics
# Homework 4
# Nora Quick

library(mclust)

# Import physio csvs
physio <- read.csv('PhysioData.csv')
head(physio)

# Problem 1
# --------------
# Part (a) / (b)

physio.cor <- cor(physio)
physio.eig <- eigen(physio.cor)

load.pcfa <- physio.eig$vec %*% diag(sqrt(physio.eig$val))
load.pcfa

# m = 2
m <- 2
uni.pcfa2 <- diag(physio.cor - load.pcfa[,1:m] %*% t(load.pcfa[,1:m]))
uni.pcfa2

# Correlation matrix
fit.pcfa2 <- load.pcfa[,1:m] %*% t(load.pcfa[,1:m]) + diag(uni.pcfa2)
fit.pcfa2

# Residual matrix
res.pcfa2 <- physio.cor - fit.pcfa2
res.pcfa2

# m = 3
m <- 3

uni.pcfa3 <- diag(physio.cor - load.pcfa[,1:m] %*% t(load.pcfa[,1:m]))
uni.pcfa3

# Correlation matrix
fit.pcfa3 <- load.pcfa[,1:m] %*% t(load.pcfa[,1:m]) + diag(uni.pcfa3)
fit.pcfa3

# Residual matrix
res.pcfa3 <- physio.cor - fit.pcfa3
res.pcfa3


# Problem 1
# ---------------
# Part (c) / (d)
# m = 2
physio.mlfa2 <- factanal(covmat = as.matrix(physio), factors=2, rotation="none")
physio.mlfa2

fit.mle2 <- physio.mlfa2$load %*% t(physio.mlfa2$load) + diag(physio.mlfa2$uni)
fit.mle2

res.mle2 <- physio.cor - fit.mle2
res.mle2

# m = 3
physio.mlfa3 <- factanal(covmat = as.matrix(physio), factors=3, rotation="none")
physio.mlfa3

fit.mle3 <- physio.mlfa3$load %*% t(physio.mlfa3$load) + diag(physio.mlfa3$uni)
fit.mle3

res.mle3 <- physio.cor - fit.mle3
res.mle3

# Import track data
track <- read.csv('TrackData.csv')
head(track)

# Problem 2
# ---------------
# Part (a)
track.distEuc <- dist(track[,-1][,-1])

track.hcEuc <- hclust(track.distEuc, method="complete")
track.hsEuc <- hclust(track.distEuc, method="single")

plot(track.hcEuc, labels=track[,2], hang=-1, sub="", main="Complete Linkage")
plot(track.hsEuc, labels=track[,2], hang=-1, sub="", main="Single Linkage")

# Problem 2
# ---------------
# Part (b)
# k = 2
track.km2 <- kmeans(track[,-1][,-1], centers=2, nstart=10)

pairs(track[,-1][,-1], col=track.km2$clus+1)

table(track.km2$clus, cutree(track.hcEuc, k=2))

# k = 3
track.km3 <- kmeans(track[,-1][,-1], centers=3, nstart=10)

pairs(track[,-1][,-1], col=track.km3$clus+1)

table(track.km3$clus, cutree(track.hcEuc, k=3))

# k = 4
track.km4 <- kmeans(track[,-1][,-1], centers=4, nstart=10)

pairs(track[,-1][,-1], col=track.km4$clus+1)

table(track.km4$clus, cutree(track.hcEuc, k=4))

# Problem 2
# ---------------
# Part (d)
track.cov <- cov(track[,-1][,-1])
track.sph <- as.matrix(track[,-1][,-1]) %*% t(chol(solve(track.cov)))
track.distMah <- dist(track.sph)

track.hcMah <- hclust(track.distMah, method="complete")

plot(track.hcMah, labels=track[,2], hang=-1)




