# ST 558: Multivariate Analytics
# Module 3 R Activity Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('C:/Users/sgrrr/Desktop')

# Read in 'SweatData.csv' dataset 

sweat <- read.csv('SweatData.csv')

###########
# Step 3  #
###########

# Define two different null hypothesis population mean vectors

mu0.1 <- c(4.1, 42.0, 10.5)
mu0.2 <- c(5.3, 43.2, 10.5)

# Calculate the sample mean

sweat.xbar <- apply(sweat, 2, mean)

# Calculate the variances of each variable

sweat.vars <- apply(sweat, 2, var)

# Produce pairs plots of the sweat data.  First, construct an augmented
# data matrix that includes the sample mean and the two hypothesized
# population mean vectors.  This is done so that the sample and hypothesized
# means will be included in the pairs plot.  We designate the sample mean
# with a solid red point (col=2, pch=16), mu0.1 with a solid green point 
# (col=3, pch=16), and mu0.2 with a solid blue point (col=4, pch=16).  
# The data points are all open black points (col=1, pch=1).

sweat.aug <- rbind(sweat, sweat.xbar, mu0.1, mu0.2)

pairs(sweat.aug, col=rep(1:4, c(20, 1, 1, 1)), pch=rep(c(1, 16), c(20, 3)))

# Identify the sample size and dimension of this data-set.

n <- nrow(sweat)
p <- ncol(sweat)
n
p


# Perform univarite tests of the null hypotheses for each variable
# separately. We can do all three univariate tests for each null hypothesis
# at the same time using R's facility with vector operations.

ts.1 <- (sweat.xbar - mu0.1)/sqrt(sweat.vars/n)
ts.2 <- (sweat.xbar - mu0.2)/sqrt(sweat.vars/n)

ts.1
ts.2

t.test(sweat[,1], mu=mu0.2[1])
t.test(sweat[,2], mu=mu0.2[2])
t.test(sweat[,3], mu=mu0.2[3])

# Find the critical value for the univariate tests as the alpha.star/2
# upper quantile of the t distribution with (n-1) degrees of freedom.

# Specify the desired test level

alpha <- 0.05

alpha.star <- alpha/p
crit.t <- qt(alpha.star/2, df=n-1, lower.tail=FALSE)
crit.t


# Examine whether any of the univariate hypotheses are rejected for 
# mu0.1 or mu0.2

abs(ts.1) > crit.t
any(abs(ts.1) > crit.t)

abs(ts.2) > crit.t
any(abs(ts.2) > crit.t)


# Calculate univariate p-values, convert to Bonferroni-correct p-values:

ps.1.raw <- 2*(1-pt(abs(ts.1), n-1))
ps.2.raw <- 2*(1-pt(abs(ts.2), n-1))

ps.1.raw
ps.2.raw

ps.1.bon <- ps.1.raw*p
ps.2.bon <- ps.2.raw*p

ps.1.bon
ps.2.bon

###########
# Step 4  #
###########

# Compute the Hotelling's T^2 statistics to test the null hypotheses
# mu0.1 and mu0.2

# Calculate the covariance matrix

sweat.cov <- cov(sweat)


T.1 <- n*t((sweat.xbar - mu0.1)) %*% solve(sweat.cov) %*% (sweat.xbar - mu0.1)
T.2 <- n*t((sweat.xbar - mu0.2)) %*% solve(sweat.cov) %*% (sweat.xbar - mu0.2)

T.1
T.2

T.1scale <- (n-p)/((n-1)*p)*T.1
T.2scale <- (n-p)/((n-1)*p)*T.2

T.1scale
T.2scale

# Find the critical value as the appropriately scaled quantile
# of the F distribution with (p, n-p) degrees of freedom.

crit.F <- qf(1-alpha, df1=p, df2=n-p)
crit.F

# Examine whether the null hypotheses mu0.1 and mu0.2 are rejected
# (reject if larger than the critical vale)

T.1scale > crit.F
T.2scale > crit.F

# Compute the p-values corresponding to each test statistic

p.1 <- 1 - pf(T.1scale, df1=p, df2=n-p)
p.2 <- 1 - pf(T.2scale, df1=p, df2=n-p)

p.1
p.2

###########
# Step 5  #
###########

install.packages('ICSNP')
library(ICSNP)

help(HotellingsT2)

HotellingsT2(sweat, mu=mu0.1)
T.1scale
p.1

HotellingsT2(sweat, mu=mu0.2)
T.2scale
p.2


install.packages('rrcov')
library(rrcov)
help(T2.test)

T2.test(sweat, mu=mu0.1)
T.1
T.1scale
p.1

T2.test(sweat, mu=mu0.2)
T.2
T.2scale
p.2

###########
# Step 6  #
###########

sweat.pair <- read.csv("SweatPairedData.csv")
head(sweat.pair)

sweat.diffs <- sweat.pair[,4:6] - sweat.pair[,1:3]
head(sweat.diffs)

n <- nrow(sweat.diffs)
p <- ncol(sweat.diffs)

mu0 <- rep(0, 3)

sweat.diffs.mean <- apply(sweat.diffs, 2, mean)
sweat.diffs.vars <- apply(sweat.diffs, 2, var)

tstats <- sqrt(n)*(sweat.diffs.mean - mu0)/sqrt(sweat.diffs.vars)
pvals <- 2*(1 - pt(abs(tstats), n-1))

tstats
pvals

pvals.bonf <- pvals*p
pvals.bonf

apply(sweat.diffs, 2, t.test)
T2.test(sweat.diffs, mu=mu0)
T2.test(sweat.diffs) 

# Note that this is the same, since the default null hypothesis
# is the zero vector.

###########
# Step 7  #
###########
  
# adrenaline data to illustrate repeated measures

adren <- read.csv("AdrenalineData.csv")
head(adren)

adren <- as.matrix(adren)

contrastMat <- rbind(c(-1, 1, 0, 0), c(-1, 0, 1, 0), c(-1, 0, 0, 1))
contrastMat

contrastObs <- adren %*% t(contrastMat) # Note the transpose!!
head(contrastObs)

contrastObs.mean <- apply(contrastObs, 2, mean)
contrastObs.cov <- cov(contrastObs)

p <- ncol(contrastObs)
n <- nrow(contrastObs)

Tstat.scaled <- (n-p)/((n-1)*p)*n*(contrastObs.mean) %*% solve(contrastObs.cov) %*% contrastObs.mean
crit.F <- qf(1 - 0.05, p, n-p)
pval <- 1 - pf(Tstat.scaled, p, n-p)

Tstat.scaled
crit.F
pval

T2.test(contrastObs, mu=rep(0,p))


