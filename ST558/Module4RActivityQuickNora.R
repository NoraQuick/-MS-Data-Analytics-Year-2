# ST 558: Multivariate Analytics
# Module 4 R Activity Example Script File
# Nora Quick

###########
# Step 2  #
###########

# Question 1: 
# -----------
seeds <- read.csv('SeedsData.csv')
head(seeds)

# Question 2: 
# -----------
pollut <- read.csv('PollutionData.csv')
head(pollut)


###########
# Step 3  #
###########

# Question 3: 
# -----------
seedV1 <- seeds[seeds$Variety==1,1:4]
seedV2 <- seeds[seeds$Variety==2,1:4]

n1 <- nrow(seedV1)
n2 <- nrow(seedV2)

p <- ncol(seedV1)

seedV1.mean <- apply(seedV1, 2, mean)
seedV2.mean <- apply(seedV2, 2, mean)

seedV1.cov <- cov(seedV1)
seedV2.cov <- cov(seedV2)

pooled.cov <- (seedV1.cov*(n1-1) + seedV2.cov*(n2-1))/(n1+n2-2)

T2stat.eq <- (seedV1.mean - seedV2.mean) %*% solve((1/n1 + 1/n2)*pooled.cov) %*% (seedV1.mean - seedV2.mean)
T2stat.eq

T2stat.eq.scale <- (n1 + n2 - p - 1)/((n1 + n2 - 2)*p)*T2stat.eq
T2stat.eq.scale

# Question 4: 
# -----------
pval.eq <- 1 - pf(T2stat.eq.scale, p, n1+n2-p-1)
pval.eq


###########
# Step 4  #
###########

library(ICSNP)
library(rrcov)

# Question 6: 
# -----------
HotellingsT2(seedV1, seedV2)

# Question 7: 
# -----------
T2.test(seedV1, seedV2)

T2stat.eq
T2stat.eq.scale
pval.eq


###########
# Step 5  #
###########

# Question 8: 
# -----------
T2stat.un <- (seedV1.mean - seedV2.mean) %*% solve(seedV1.cov/n1 + seedV2.cov/n2) %*% (seedV1.mean - seedV2.mean)
T2stat.un

# Question 9: 
# -----------
pval.un <- 1 - pchisq(T2stat.un, p)
pval.un


###########
# Step 6  #
###########

# Question 11: 
# -----------
seedV1 <- seeds[seeds$Variety==1,1:4]
seedV2 <- seeds[seeds$Variety==2,1:4]
seedV3 <- seeds[seeds$Variety==3,1:4]

n1 <- nrow(seedV1)
n2 <- nrow(seedV2)
n3 <- nrow(seedV3)

seedV1.cov <- cov(seedV1)
seedV2.cov <- cov(seedV2)
seedV3.cov <- cov(seedV3)

Ntot <- n1 + n2 + n3

seeds.cov <- cov(seeds[,1:4])

WW <- (n1-1)*seedV1.cov + (n2-1)*seedV2.cov + (n3-1)*seedV3.cov

TT <- (Ntot-1)*seeds.cov

LamStat <- det(WW)/det(TT) 
LamStat

# Question 12: 
# -----------
p <- ncol(seedV1)
k <- length(unique(seeds$Variety))

LamStat.scaled <- -(Ntot - 1 - (p+k)/2)*log(LamStat)
LamStat.scaled

# Question 13: 
# -----------
1 - pchisq(LamStat.scaled, p*(k-1))


###########
# Step 7  #
###########

# Question 15: 
# -----------
Wilks.test(seeds[,1:4], grouping=seeds$Variety)

seeds.manova <- manova(as.matrix(seeds[,1:4]) ~ seeds$Variety)
seeds.manova

summary(seeds.manova)
summary(seeds.manova, test='Wilks')

###########
# Step 8  #
###########

# Question 16: 
# -----------
pollut <- as.matrix(pollut)

pollut.y <- pollut[,1:2]
pollut.x <- pollut[,3:4]
head(pollut.y)
head(pollut.x)

mod.all <- lm(pollut.y ~ pollut.x)

mod.all

summary(mod.all)

# Question 17: 
# -----------
mod.0 <- lm(pollut.y ~ 1)

anova(mod.all, mod.0)

mod.2 <- lm(pollut.y ~ pollut.x[,-2])

anova(mod.all, mod.2)
