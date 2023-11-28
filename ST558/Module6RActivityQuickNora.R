# ST 558: Multivariate Analytics
# Module 6 R Activity 
# Nora Quick

###########
# Step 2  #
###########

library(CCA)

# Question 1: 
# -----------
psych <- read.csv('PsychData.csv')
head(psych)

# Question 2: 
# -----------
pairs(psych[,1:6])

# Question 3: 
# -----------
seeds <- read.csv('Seeds3Data.csv')
head(seeds)

# Question 4: 
# -----------
pairs(seeds[,1:7])


###########
# Step 3  #
###########

# Question 5: 
# -----------
x1 <- as.matrix(psych[,1:3])
x2 <- as.matrix(psych[,4:6])

sig11 <- cov(x1)
sig22 <- cov(x2)
sig12 <- cov(x1, x2)

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

# Question 6: 
# -----------
u1 <- x1 %*% t(a1)
v1 <- x2 %*% t(b1)

plot(u1, v1)

# Question 7: 
# -----------
sqrt(A1.eig$val[1])
sqrt(A2.eig$val[1])
cor(u1, v1)

###########
# Step 4  #
###########

# Question 8: 
# -----------
psych.cc <- cc(psych[,1:3], psych[,4:6])
names(psych.cc)

u1[1:10]
psych.cc$scores$xscores[1:10,1]

u1.sc <- u1 - mean(u1)
u1.sc[1:10]

psych.cc$scores$yscores[1:10,1]
v1.sc <- v1 - mean(v1)
v1.sc[1:10]

###########
# Step 5  #
###########

# Question 9: 
# -----------
seeds.cov <- cov(seeds)

seeds.cov.eig <- eigen(seeds.cov)
seeds.cov.eig

par(mfrow=c(3,3), oma=c(0,0,2,0))
for(i in 1:7){
  plot(1:7, seeds.cov.eig$vec[,i], xlab="Variable", ylab="Loading", main=paste("Principal Component ", i, sep=""), ylim=c(-1, 1))
  abline(h=0)
}
mtext("Unstandardized Principal Components", outer=T)

par(mfrow=c(1,1), oma=c(0,0,0,0))

seeds.pcscores.raw12 <- as.matrix(seeds) %*% seeds.cov.eig$vec[,1:2]
plot(seeds.pcscores.raw12, xlab="First Principal Component Scores", ylab="Second Principal Component Scores", main="(Unstandardized) Score Plot")

par(mfrow=c(1,1), oma=c(0,0,0,0))
plot(1:7, seeds.cov.eig$val, type="b", xlab="Component", ylab="Variance", main="Scree Plot for NYSE Data")

seeds.cov.eig$val
cumsum(seeds.cov.eig$val)
cumsum(seeds.cov.eig$val)/sum(seeds.cov.eig$val)

plot(1:7, cumsum(seeds.cov.eig$val)/sum(seeds.cov.eig$val), type="b", xlab="# Components", ylab="Cumulative Variance Explained")


###########
# Step 6  #
###########

# Question 12: 
# -----------
seeds.cor <- cor(seeds)
seeds.cor.eig <- eigen(seeds.cor)

seeds.pc1 <- prcomp(seeds)
names(seeds.pc1)

seeds.pc1.sc <- prcomp(seeds, scale=T)

seeds.pc1.sc$sdev
sqrt(seeds.cor.eig$val)

seeds.pc1.sc$rotation
seeds.cor.eig$vec



