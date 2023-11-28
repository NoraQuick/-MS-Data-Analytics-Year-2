# ST 558: Multivariate Analytics
# Homework 2
# Nora Quick

# Import test scores csv
testscores <- read.csv('TestScores.csv')
head(testscores)

# Problem 1
# --------------
# Part (a)
mu0.1 <- c(500, 50, 30)

testscores.xbar <- apply(testscores, 2, mean)
testscores.vars <- apply(testscores, 2, var)

n <- nrow(testscores)
p <- ncol(testscores)
n
p

ts.1 <- (testscores.xbar - mu0.1)/sqrt(testscores.vars/n)
ts.1

alpha <- 0.05

alpha.star <- alpha/p
crit.t <- qt(alpha.star/2, df=n-1, lower.tail=FALSE)
crit.t

abs(ts.1) > crit.t
any(abs(ts.1) > crit.t)

ps.1.raw <- 2*(1-pt(abs(ts.1), n-1))
ps.1.raw

ps.1.bon <- ps.1.raw*p
ps.1.bon


# Problem 1
# ---------------
# Part (b)
testscores.cov <- cov(testscores)

T.1 <- n*t((testscores.xbar - mu0.1)) %*% solve(testscores.cov) %*% (testscores.xbar - mu0.1)
T.1

T.1scale <- (n-p)/((n-1)*p)*T.1
T.1scale

crit.F <- qf(1-alpha, df1=p, df2=n-p)
crit.F

T.1scale > crit.F

p.1 <- 1 - pf(T.1scale, df1=p, df2=n-p)
p.1



# Import reading test csv
readingtest <- read.csv('ReadingTest.csv')
head(readingtest)

# Problem 2
# ---------------
# Part (b)
readingtest.diffs <- readingtest[,4:5] - readingtest[,2:3]
head(readingtest.diffs)

n <- nrow(readingtest.diffs)
p <- ncol(readingtest.diffs)

mu0 <- rep(0, 2)

readingtest.diffs.mean <- apply(readingtest.diffs, 2, mean)
readingtest.diffs.vars <- apply(readingtest.diffs, 2, var)

tstats <- sqrt(n)*(readingtest.diffs.mean - mu0)/sqrt(readingtest.diffs.vars)
pvals <- 2*(1 - pt(abs(tstats), n-1))
tstats
pvals

pvals.bonf <- pvals*p
pvals.bonf

# Problem 2
# ----------------
# Part (c)
T2.test(readingtest.diffs, mu=mu0)

# Problem 2
# ----------------
# Part (d)
T2.test(readingtest.diffs, mu=c(1,1))


# Import skull data csv
skulldata <- read.csv('SkullData.csv')
head(skulldata)

# Problem 3
# ----------------
# Part (a)
skullT1 <- skulldata[skulldata$Year==-4000,1:4]
skullT2 <- skulldata[skulldata$Year==-3300,1:4]
skullT3 <- skulldata[skulldata$Year==-1850,1:4]
skullT4 <- skulldata[skulldata$Year==-200,1:4]
skullT5 <- skulldata[skulldata$Year==150,1:4]

skullT1
skullT2
skullT3
skullT4
skullT5

skullT1.cov <- cov(skullT1)
skullT2.cov <- cov(skullT2)
skullT3.cov <- cov(skullT3)
skullT4.cov <- cov(skullT4)
skullT5.cov <- cov(skullT5)

skullT1.cov
skullT2.cov
skullT3.cov
skullT4.cov
skullT5.cov

# Problem 3
# ----------------
# Part (b)
p <- 4

anova(lm(Year ~ factor(Year), data = skulldata))$P[1] * p
anova(lm(MB ~ factor(Year), data = skulldata))$P[1] * p
anova(lm(BH ~ factor(Year), data = skulldata))$P[1] * p
anova(lm(BL ~ factor(Year), data = skulldata))$P[1] * p

alphastar <- alpha/p
alphastar

# Problem 3
# ----------------
# Part (c)
n1 <- nrow(skullT1)
n2 <- nrow(skullT2)
n3 <- nrow(skullT3)
n4 <- nrow(skullT4)
n5 <- nrow(skullT5)

Ntot <- n1 + n2 + n3 + n4 + n5

skull.cov <- cov(skulldata[,1:4])

WW <- (n1-1)*skullT1.cov + (n2-1)*skullT2.cov + (n3-1)*skullT3.cov + (n4-1)*skullT4.cov + (n5-1)*skullT5.cov
TT <- (Ntot-1)*skull.cov

LamStat <- det(WW)/det(TT)
LamStat

p <- ncol(skullT1)
k <- length(unique(skulldata$Year))

LamStat.scaled <- -(Ntot - 1 - (p+k)/2)*log(LamStat)
LamStat.scaled

1 - pchisq(LamStat.scaled, p*(k-1))





