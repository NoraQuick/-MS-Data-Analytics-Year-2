# ST 558: Multivariate Analytics
# Module 5 R Activity Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 558 2017/Datasets')

# Install and load the 'MASS', 'class', 'rpart', and 'rpart.plot'
# R libraries

install.packages('MASS')
install.packages('class')
install.packages('rpart')
install.packages('rpart.plot')

library(MASS)
library(class)
library(rpart)
library(rpart.plot)

# Read in 'IrisData.csv' and 'VertebralColumnData.csv' datasets

iris <- read.csv('IrisData.csv')
vert <- read.csv('VertebralColumnData.csv')

###########
# Step 3  #
###########

# Multi-group LDA

# Separate types

irisT1 <- iris[iris$Type==1,1:4]
irisT2 <- iris[iris$Type==2,1:4]
irisT3 <- iris[iris$Type==3,1:4]

# Calculate sample size for each type

n1 <- nrow(irisT1)
n2 <- nrow(irisT2)
n3 <- nrow(irisT3)

# Calculate sample covariance matrix for each type

irisT1.cov <- cov(irisT1)
irisT2.cov <- cov(irisT2)
irisT3.cov <- cov(irisT3)

# Calculate sample mean vector for each type

irisT1.xbar <- apply(irisT1, 2, mean)
irisT2.xbar <- apply(irisT2, 2, mean)
irisT3.xbar <- apply(irisT3, 2, mean)

# # # # # # # # # # #

# Calculate pooled covariance matrix from the different groups

iris.Sp <- ((n1-1)*irisT1.cov + (n2-1)*irisT2.cov + (n3-1)*irisT3.cov)/(n1+n1+n3-3)

# Calculate combined covariance matrix from entire dataset

iris.S <- cov(iris[,1:4])

# Construct matrices W, T, and B using relationships between W and 
# pooled covariance, and between T and total combined covariance.

Wmat <- (n1+n2+n3-3)*iris.Sp
Tmat <- (nrow(iris)-1)*iris.S
Bmat <- Tmat - Wmat

# Construct W^(-1)B matrix, and use 'eigen()' function to find its
# eigenvalues and eigenvectors

WinvB <- solve(Wmat) %*% Bmat
WinvB.eigen <- eigen(WinvB)

# First eigenvector/first linear discriminant function coefficients

v1 <- WinvB.eigen$vec[,1]
v1

# Second eigenvector/first linear discriminant function coefficients

v2 <- WinvB.eigen$vec[,2]
v2

# Calculate linear discriminant function values for each observation

Y1vals <- as.matrix(iris[,1:4]) %*% v1
Y2vals <- as.matrix(iris[,1:4]) %*% v2

# Plot linear discriminant function values, with points colored by Type

plot(Y1vals, Y2vals, col=iris$Type+1, xlab=expression(Y[1]), ylab=expression(Y[2]))

# Calculate proportion of group separation explained by first linear
# discriminant direction

WinvB.eigen$val[1:2]/sum(WinvB.eigen$val)

###########
# Step 4  #
###########

# lda function

iris.lda <- lda(Type ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
iris.lda

# Scale the linear discriminant functions by multiplying each coefficient
# vector by the inverse of v_j^T S_p v_j

d1 <- sqrt(1/(v1 %*% iris.Sp %*% v1))*v1
d2 <- sqrt(1/(v2 %*% iris.Sp %*% v2))*v2

d1
d2

iris.lda$scal

###########
# Step 5  #
###########

iris.newObs <- data.frame("Sepal.Length"=7.5, "Sepal.Width"=4.0, "Petal.Length"=5.0, "Petal.Width"=1.0)
iris.newObs.v <- as.numeric(iris.newObs)

# Prediction with Multi-group LDA

discVal1 <- t(iris.newObs.v-irisT1.xbar) %*% solve(iris.Sp) %*% (iris.newObs.v-irisT1.xbar)
discVal2 <- t(iris.newObs.v-irisT2.xbar) %*% solve(iris.Sp) %*% (iris.newObs.v-irisT2.xbar)
discVal3 <- t(iris.newObs.v-irisT3.xbar) %*% solve(iris.Sp) %*% (iris.newObs.v-irisT3.xbar)

discVal1
discVal2
discVal3

# # # # #

iris.newObs.ldaPred <- predict(iris.lda, newdata=iris.newObs)

iris.newObs.ldaPred

###########
# Step 6  #
###########

# Multi-group QDA distances

qdiscT1 <- t(iris.newObs.v - irisT1.xbar) %*% solve(irisT1.cov) %*% (iris.newObs.v - irisT1.xbar)
qdiscT2 <- t(iris.newObs.v - irisT2.xbar) %*% solve(irisT2.cov) %*% (iris.newObs.v - irisT2.xbar)
qdiscT3 <- t(iris.newObs.v - irisT3.xbar) %*% solve(irisT3.cov) %*% (iris.newObs.v - irisT3.xbar)

c(qdiscT1, qdiscT2, qdiscT3)

# Modified mult-group QDA distances, assuming multivariate normal
# population distribution and equal prior probabilities in all groups

priorVals <- c(1/3, 1/3, 1/3)

qdiscT1.mod <- -2*log(priorVals[1]) + log(det(irisT1.cov)) + qdiscT1
qdiscT2.mod <- -2*log(priorVals[2]) + log(det(irisT2.cov)) + qdiscT2
qdiscT3.mod <- -2*log(priorVals[3]) + log(det(irisT3.cov)) + qdiscT3

c(qdiscT1.mod, qdiscT2.mod, qdiscT3.mod)

# # # # # 

# qda function

iris.qda <- qda(Type ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)

iris.newObs <- data.frame("Sepal.Length"=7.5, "Sepal.Width"=4.0, "Petal.Length"=5.0, "Petal.Width"=1.0)

iris.newObs.qdaPred <- predict(iris.qda, newdata=iris.newObs)

iris.newObs.qdaPred

###########
# Step 7  #
###########

# k-Nearest Neighbors

head(vert)
nrow(vert)

ntrain <- 200

set.seed(1234567)

train.rows <- sample(1:310, ntrain, replace=F)
train.rows <- sort(train.rows)

vert.train <- vert[train.rows,]
vert.test <- vert[-train.rows,]

dim(vert.train)
dim(vert.test)

head(vert.train)
head(vert.test)

# Fit knn model with k = 5

knnRslt.5 <- knn(vert.train[,1:6], vert.test[,1:6], cl=vert.train$ClassLabel, k=5)
knnRslt.5

# Compare true labels to predicted labels

table(vert.test$ClassLabel, knnRslt.5)

# Calculate classification error rate on test set

sum(vert.test$ClassLabel != knnRslt.5)/length(knnRslt.5)
mean(vert.test$ClassLabel != knnRslt.5)

###########
# Step 8  #
###########

# CART

vert.tree <- rpart(ClassLabel ~ PelvicIncidence + PelvicTilt + LumbarLordosisAngle + SacralSlope + PelvicRadius + GradeSpond, control=rpart.control(minsplit=30, minbucket=5), data=vert.train)

vert.tree
summary(vert.tree)

prp(vert.tree, type=1, digits=4, extra=1, varlen=0)

vert.tree.testPred <- predict(vert.tree, vert.test[,1:6])
head(vert.tree.testPred)

vert.tree.testPredCl <- predict(vert.tree, vert.test[,1:6], type="class")

table(vert.test$ClassLabel, vert.tree.testPredCl)

mean(vert.test$ClassLabel != vert.tree.testPredCl)


