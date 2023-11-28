# ST 558: Multivariate Analytics
# Module 3 R Activity : Nora Quick

# Question 1
# -----------
hrbp <- read.csv('HRBPData.csv')
head(hrbp)


# Question 2
# -----------
mu0.1 <- c(70, 80, 120)

hrbp.xbar <- apply(hrbp, 2, mean)
hrbp.vars <- apply(hrbp, 2, var)

hrbp.aug <- rbind(hrbp, hrbp.xbar, mu0.1)

pairs(hrbp.aug, col=rep(1:3, c(20,1,1)), pch=rep(c(1,16), c(20,3)))


# Question 3
# -----------
n <- nrow(hrbp)
p <- ncol(hrbp)

ts.1 <- (hrbp.xbar - mu0.1)/sqrt(hrbp.vars/n)
ts.1


# Question 4
# -----------
ps.1.raw <- 2*(1-pt(abs(ts.1), n-1))
ps.1.raw

ps.1.bon <- ps.1.raw*p
ps.1.bon

# Question 5
# -----------
alpha <- 0.05

alpha.star <- alpha/p
crit.t <- qt(alpha.star/2, df=n-1, lower.tail=FALSE)
crit.t

abs(ts.1) > crit.t
any(abs(ts.1) > crit.t)


# Question 6
# -----------
hrbp.cov <- cov(hrbp)

T.1 <- n*t((hrbp.xbar - mu0.1)) %*% solve(hrbp.cov) %*% (hrbp.xbar - mu0.1)
T.1

T.1scale <- (n-p)/((n-1)*p)*T.1
T.1scale

crit.F <- qf(1-alpha, df1=p, df2=n-p)
crit.F

T.1scale > crit.F

p.1 <- 1 - pf(T.1scale, df1=p, df2=n-p)
p.1

# Question 7
# -----------
library(rrcov)
T2.test(hrbp, mu=mu0.1)

# Question 8
# -----------
hrbpPaired <- read.csv('HRBPPairedData.csv')
head(hrbpPaired)

# Question 9
# -----------
hrbpPaired.diffs <- hrbpPaired[,4:6] - hrbpPaired[,1:3]
head(hrbpPaired.diffs)

n <- nrow(hrbpPaired.diffs)
p <- ncol(hrbpPaired.diffs)

mu0 <- rep(0, 3)

hrbpPaired.diffs.mean <- apply(hrbpPaired.diffs, 2, mean)
hrbpPaired.diffs.vars <- apply(hrbpPaired.diffs, 2, var)

tstats <- sqrt(n)*(hrbpPaired.diffs.mean - mu0)/sqrt(hrbpPaired.diffs.vars)
pvals <- 2*(1 - pt(abs(tstats), n-1))

tstats
pvals

pvals.bonf <- pvals*p
pvals.bonf

alpha <- 0.05
alpha.star <- alpha/p
crit.t <- qt(alpha.star/2, df=n-1, lower.tail=FALSE)
crit.t

abs(tstats) > crit.t
any(abs(tstats) > crit.t)


# Question 10
# -----------
T2.test(hrbpPaired.diffs, mu=mu0)

# Question 11
# -----------
hrRepMeas <- read.csv('HRBPPairedData.csv')
head(hrRepMeas)

# Question 12
# -----------
hrRepMeasMat <- as.matrix(hrRepMeas)

contrastMat <- rbind(c(-1, 1, 0, 0, 0, 0), c(-1, 0, 1, 0, 0, 0), c(-1, 0, 0, 1, 0, 0), c(-1, 0, 0, 0, 1, 0), c(-1, 0, 0, 0, 0, 1))
contrastMat

contrastObs <- hrRepMeasMat %*% t(contrastMat)
head(contrastObs)

# Question 13
# -----------
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

# Question 14
# -----------
contrastMat <- rbind(c(0, 0, 0, 0, 1, -1), c(0, 0, 0, 1, 0, -1), c(0, 0, 1, 0, 0, -1), c(0, 1, 0, 0, 1, -1), c(1, 0, 0, 0, 0, -1))
contrastMat

contrastObs <- hrRepMeasMat %*% t(contrastMat)
contrastObs

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

