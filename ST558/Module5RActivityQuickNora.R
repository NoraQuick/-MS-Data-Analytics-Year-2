# ST 558: Multivariate Analytics
# Module 5 R Activity Example Script File
# Nora Quick

###########
# Step 2  #
###########

library(MASS)
library(class)
library(rpart)
library(rpart.plot)

# Question 1: 
# -----------
wine <- read.csv('BalWineData.csv')
head(wine)

# Question 2: 
# -----------
pairs(wine[,1:6], col=wine$Variety)


###########
# Step 3  #
###########

# Question 3: 
# -----------
wineV1 <- wine[wine$Variety==1,1:6]
wineV2 <- wine[wine$Variety==2,1:6]
wineV3 <- wine[wine$Variety==3,1:6]

n1 <- nrow(wineV1)
n2 <- nrow(wineV2)
n3 <- nrow(wineV3)

wineV1.cov <- cov(wineV1)
wineV2.cov <- cov(wineV2)
wineV3.cov <- cov(wineV3)

wineV1.xbar <- apply(wineV1, 2, mean)
wineV2.xbar <- apply(wineV2, 2, mean)
wineV3.xbar <- apply(wineV3, 2, mean)

wine.Sp <- ((n1-1)*wineV1.cov + (n2-1)*wineV2.cov + (n3-1)*wineV3.cov)/(n1+n1+n3-3)
wine.S <- cov(wine[,1:6])

Wmat <- (n1+n2+n3-3)*wine.Sp
Tmat <- (nrow(wine)-1)*wine.S
Bmat <- Tmat - Wmat

WinvB <- solve(Wmat) %*% Bmat
WinvB.eigen <- eigen(WinvB)

v1 <- WinvB.eigen$vec[,1]
v1

v2 <- WinvB.eigen$vec[,2]
v2

# Question 4: 
# -----------
Y1vals <- as.matrix(wine[,1:6]) %*% v1
Y2vals <- as.matrix(wine[,1:6]) %*% v2

plot(Y1vals, Y2vals, col=wine$Variety+1, xlab=expression(Y[1]), ylab=expression(Y[2]))


###########
# Step 4  #
###########

# Question 5: 
# -----------
wine.lda <- lda(Variety ~ Alcohol + MalicAcid + Ash + Magnesium + TotalPhenols + Flavonoids, data=wine)
wine.lda


###########
# Step 5  #
###########

# Question 6: 
# -----------
wine.newObs <- data.frame("Alcohol"=14.0, "MalicAcid"=5.0, "Ash"=2.0, "Magnesium"=120.0, "TotalPhenols"=3.0, "Flavonoids"=4.0)
wine.newObs.v <- as.numeric(wine.newObs)

wine.newObs
wine.newObs.v

discVal1 <- t(wine.newObs.v-wineV1.xbar) 
discVal2 <- t(wine.newObs.v-wineV2.xbar) %*% solve(wine.Sp) %*% (wine.newObs.v-wineV2.xbar)
discVal3 <- t(wine.newObs.v-wineV3.xbar) %*% solve(wine.Sp) %*% (wine.newObs.v-wineV3.xbar)

discVal1
discVal2
discVal3

wine.newObs.ldaPred <- predict(wine.lda, newdata=wine.newObs)
wine.newObs.ldaPred


###########
# Step 6  #
###########

# Question 7: 
# -----------
qdiscT1 <- t(wine.newObs.v - wineV1.xbar) %*% solve(wineV1.cov) %*% (wine.newObs.v - wineV1.xbar)
qdiscT2 <- t(wine.newObs.v - wineV2.xbar) %*% solve(wineV2.cov) %*% (wine.newObs.v - wineV2.xbar)
qdiscT3 <- t(wine.newObs.v - wineV3.xbar) %*% solve(wineV3.cov) %*% (wine.newObs.v - wineV3.xbar)

c(qdiscT1, qdiscT2, qdiscT3)

# Question 8: 
# -----------
priorVals <- c(1/3, 1/3, 1/3)

qdiscT1.mod <- -2*log(priorVals[1]) + log(det(wineV1.cov)) + qdiscT1
qdiscT2.mod <- -2*log(priorVals[2]) + log(det(wineV2.cov)) + qdiscT2
qdiscT3.mod <- -2*log(priorVals[3]) + log(det(wineV3.cov)) + qdiscT3

c(qdiscT1.mod, qdiscT2.mod, qdiscT3.mod)

wine.qda <- qda(Variety ~ Alcohol + MalicAcid + Ash + Magnesium + TotalPhenols + Flavonoids, data=wine)

wine.newObs <- data.frame("Alcohol"=14.0, "MalicAcid"=5.0, "Ash"=2.0, "Magnesium"=120.0, "TotalPhenols"=3.0, "Flavonoids"=4.0)

wine.newObs.qdaPred <- predict(wine.qda, newdata=wine.newObs)
wine.newObs.qdaPred


###########
# Step 7  #
###########

# Question 9: 
# -----------
nrow(wine)

ntrain <- 90

set.seed(1234567)

train.rows <- sample(1:135, ntrain, replace=F)
train.rows <- sort(train.rows)

wine.train <- wine[train.rows,]
wine.test <- wine[-train.rows,]

dim(wine.train)
dim(wine.test)

head(wine.train)
head(wine.test)

# Question 10: 
# -----------
knnRslt.5 <- knn(wine.train[,1:6], wine.test[,1:6], cl=wine.train$Variety, k=5)
knnRslt.5

table(wine.test$Variety, knnRslt.5)

# Question 11: 
# -----------
knnRslt.15 <- knn(wine.train[,1:6], wine.test[,1:6], cl=wine.train$Variety, k=15)
knnRslt.15

table(wine.test$Variety, knnRslt.15)

###########
# Step 8  #
###########

# Question 12: 
# -----------
wine.tree <- rpart(factor(Variety) ~ Alcohol + MalicAcid + Ash + Magnesium + TotalPhenols + Flavonoids, control=rpart.control(minsplit=30, minbucket=5), data=wine.train)

wine.tree
summary(wine.tree)

#prp(wine.tree, type=1, digits=4, extra=1, varlen=0)

wine.tree.testPred <- predict(wine.tree, wine.test[,1:6])
head(wine.tree.testPred)

wine.tree.testPredCl <- predict(wine.tree, wine.test[,1:6], type="vector")
wine.tree.testPredCl[1:6]

table(wine.test$Variety, wine.tree.testPredCl)

