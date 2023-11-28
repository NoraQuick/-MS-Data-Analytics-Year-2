# ST 558: Multivariate Analytics
# Homework 3
# Nora Quick

library(MASS)
library(class)
library(rpart)
library(rpart.plot)

# Import glass csvs
glasstrain <- read.csv('GlassDataTrain.csv')
head(glasstrain)

glasstest <- read.csv('GlassDataTest.csv')
head(glasstest)

# Problem 1
# --------------
# Part (a)
train.lda <- lda(NewType ~ RI + Na + Al + Si + Ca, data=glasstrain)
train.lda

train.newObs.ldaPred <- predict(train.lda, newdata=glasstest)
train.newObs.ldaPred

train.newObs.ldaPred$class == glasstest$NewType

17/64

# Problem 1
# ---------------
# Part (b)
train.qda <- qda(NewType ~ RI + Na + Al + Si + Ca, data=glasstrain)
train.qda

train.newObs.qdaPred <- predict(train.qda, newdata=glasstest)
train.newObs.qdaPred

train.newObs.qdaPred$class == glasstest$NewType

26/64

# Problem 1
# ---------------
# Part (c)
knnRslt.5 <- knn(glasstrain[,1:5], glasstest[,1:5], cl=glasstrain$NewType, k=5)
knnRslt.5

table(glasstest$NewType, knnRslt.5)

(2+2+2+4+7)/64

# Problem 1
# ---------------
# Part (d)
train.tree <- rpart(factor(NewType) ~ RI + Na + Al + Si + Ca, control=rpart.control(minsplit=10, minbucket=5), data=glasstrain)
train.tree

train.tree.testPred <- predict(train.tree, glasstest[,1:5])
head(train.tree.testPred)

train.tree.testPredCl <- predict(train.tree, glasstest[,1:5], type="vector")

table(glasstest$NewType, train.tree.testPredCl)

(2+2+3+7)/64

# Hand input diabetic data
sig11 <- rbind(c(1106.00, 396.70, 108.40),
             c(396.70, 2382.00, 1143.00),
             c(108.40, 1143.00, 2136.00))
sig12 <- rbind(c(0.79, 26.23),
             c(-0.21, -23.96),
             c(2.19, -20.84))
sig22 <- rbind(c(0.02, 0.22),
             c(0.22, 70.56))
sig21 <- t(sig12)

# Problem 2
# ---------------
# Part (all)
sig11.eig <- eigen(sig11)
sig11.5 <- sig11.eig$vec %*% diag(sqrt(sig11.eig$val)) %*% 
  t(sig11.eig$vec)

sig11
sig11.5 %*% sig11.5

sig22.eig <- eigen(sig22)
sig22.5 <- sig22.eig$vec %*% diag(sqrt(sig22.eig$val)) %*% 
  t(sig22.eig$vec)

sig22
sig22.5 %*% sig22.5

A1 <- solve(sig11.5) %*% sig12 %*% solve(sig22) %*% t(sig12) %*% solve(sig11.5)
A2 <- solve(sig22.5) %*% t(sig12) %*% solve(sig11) %*% sig12 %*% solve(sig22.5)

A1.eig <- eigen(A1)
A2.eig <- eigen(A2)

e1 <- A1.eig$vec[,1]
f1 <- A2.eig$vec[,1]

a1 <- e1 %*% solve(sig11.5)
b1 <- f1 %*% solve(sig22.5)

a1
b1

sqrt(A1.eig$val[1])

e2 <- A1.eig$vec[,2]
f2 <- A2.eig$vec[,2]

a2 <- e2 %*% solve(sig11.5)
b2 <- f2 %*% solve(sig22.5)

a2
b2

sqrt(A1.eig$val[2])


# Import track csv
track <- read.csv('TrackData.csv')
head(track)

# Problem 3
# ---------------
# Part (a)
track.cov <- cov(track[, 3:10])
head(track.cov)

track.cor <- cor(track[, 3:10])
head(track.cor)

# Problem 3
# ---------------
# Part (b)
track.cov.eig <- eigen(track.cov)
head(track.cov.eig)

# Problem 3
# ---------------
# Part (c)
track.cor.eig <- eigen(track.cor)
head(track.cor.eig)

# Problem 3
# ---------------
# Part (d)
par(mfrow=c(3,3), oma=c(0,0,2,0))
for(i in 1:4){
  plot(1:8, track.cov.eig$vec[,i], xlab="Variable", ylab="Loading", main=paste("Principal Component ", i, sep=""), ylim=c(-1, 1))
  abline(h=0)
}
mtext("Matrix S", outer=T)

# Problem 3
# ---------------
# Part (e)
par(mfrow=c(3,3), oma=c(0,0,2,0))
for(i in 1:4){
  plot(1:8, track.cor.eig$vec[,i], xlab="Variable", ylab="Loading", main=paste("Principal Component ", i, sep=""), ylim=c(-1, 1))
  abline(h=0)
}
mtext("Matrix R", outer=T)

# Problem 3
# ---------------
# Part (i)
par(mfrow=c(1,1), oma=c(0,0,0,0))
plot(1:8, track.cor.eig$val, type="b", xlab="Component", ylab="Variance", main="Scree Plot for Matrix R")

track.cor.eig$val
cumsum(track.cor.eig$val)
cumsum(track.cor.eig$val)/sum(track.cor.eig$val)

plot(1:8, cumsum(track.cor.eig$val)/sum(track.cor.eig$val), type="b", xlab="# Components", ylab="Cumulative Variance Explained", main="Cumulative Variance Explained Plot for Matrix R")



