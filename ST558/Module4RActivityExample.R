# ST 558: Multivariate Analytics
# Module 4 R Activity Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 558 2017/Datasets')

# Read in 'IrisData.csv' dataset 

iris <- read.csv('IrisData.csv')

# Read in 'ExamData.csv'

exam <- read.csv('ExamData.csv')


###########
# Step 3  #
###########

# Separate out Types 1 and 2 to compare

irisT1 <- iris[iris$Type==1,1:4]
irisT2 <- iris[iris$Type==2,1:4]

# Perform Hotelling's T2 test (equal variance version) by hand

n1 <- nrow(irisT1)
n2 <- nrow(irisT2)

p <- ncol(irisT1)

irisT1.mean <- apply(irisT1, 2, mean)
irisT2.mean <- apply(irisT2, 2, mean)

irisT1.cov <- cov(irisT1)
irisT2.cov <- cov(irisT2)

# Pooled covariance matrix is a weighted mean of the covariance
# matrices for the two samples

pooled.cov <- (irisT1.cov*(n1-1) + irisT2.cov*(n2-1))/(n1+n2-2)

# Equal-variance two-sample Hotelling's T^2 test statistic

T2stat.eq <- (irisT1.mean - irisT2.mean) %*% solve((1/n1 + 1/n2)*pooled.cov) %*% (irisT1.mean - irisT2.mean)

T2stat.eq

# Scaled version of the T^2 statistic, to compare to F distribution

T2stat.eq.scale <- (n1 + n2 - p - 1)/((n1 + n2 - 2)*p)*T2stat.eq

T2stat.eq.scale

pval.eq <- 1 - pf(T2stat.eq.scale, p, n1+n2-p-1)
pval.eq

###########
# Step 4  #
###########

# Perform Hotelling's T2 test using 'HotellingsT2()' function in 
# 'ICSNP' package

library(ICSNP)
HotellingsT2(irisT1, irisT2)

# Perform Hotelling's T2 test using 'T2.test()' function in 
# 'rrcov' package

library(rrcov)
T2.test(irisT1, irisT2)

# Compare to what we computed by hand:

T2stat.eq
T2stat.eq.scale
pval.eq

###########
# Step 5  #
###########

# Perform Hotelling's T2 test (unequal variance version) by hand

T2stat.un <- (irisT1.mean - irisT2.mean) %*% solve(irisT1.cov/n1 + irisT2.cov/n2) %*% (irisT1.mean - irisT2.mean)

T2stat.un

pval.un <- 1 - pchisq(T2stat.un, p)

pval.un

###########
# Step 6  #
###########

# MANOVA by hand

irisT1 <- iris[iris$Type==1,1:4]
irisT2 <- iris[iris$Type==2,1:4]
irisT3 <- iris[iris$Type==3,1:4]

# Obtain the sample sizes for each group

n1 <- nrow(irisT1)
n2 <- nrow(irisT2)
n3 <- nrow(irisT3)

# Calculate sample covariance matrices for each group separately

irisT1.cov <- cov(irisT1)
irisT2.cov <- cov(irisT2)
irisT3.cov <- cov(irisT3)

# Calculate the total sample size of all groups combined

Ntot <- n1 + n2 + n3

# Calculate the sample covariance matrix for all groups combined

iris.cov <- cov(iris[,1:4])

# Calculate the 'Within Sums-of-Squares' matrix using the separate
# sample covariance matrices and sample sizes

WW <- (n1-1)*irisT1.cov + (n2-1)*irisT2.cov + (n3-1)*irisT3.cov

# Calculate the 'Total Sums-of-Squares' matrix using the combined
# sample covariance matrix and sample size

TT <- (Ntot-1)*iris.cov

# Calculate Wilks' Lambda test statistic as the ratio of the 
# determinants of the Within and Total Sums-of-Squares matrices.

LamStat <- det(WW)/det(TT)

LamStat

p <- ncol(irisT1)
k <- length(unique(iris$Type))

LamStat.scaled <- -(Ntot - 1 - (p+k)/2)*log(LamStat)

LamStat.scaled

1 - pchisq(LamStat.scaled, p*(k-1))

###########
# Step 7  #
###########

# MANOVA using the 'Wilks.test()' function in package 'rrcov'

Wilks.test(iris[,1:4], grouping=iris$Type)

# MANOVA using the 'manova()' function: NOTE that this calculates a 
# different statistic, Pillai's trace rather than Wilk's Lambda.

iris.manova <- manova(as.matrix(iris[,1:4]) ~ iris$Type)
iris.manova

summary(iris.manova)
summary(iris.manova, test='Wilks')

###########
# Step 8  #
###########

# Convert the 'exam' data to matrix so that it will work in the 'lm()' function

head(exam)
exam <- as.matrix(exam)

# Separate the 'response' variables (the first two columns) and the
# 'predictor' variables (the last three columns)

exam.y <- exam[,1:2]
exam.x <- exam[,3:5]
head(exam.y)
head(exam.x)

# Fit a linear model using all of the predictor variables

mod.all <- lm(exam.y ~ exam.x)

# Display the model fit

mod.all

# Summarize the model fit

summary(mod.all)

# Fit a linear model with only an intercept term (no predictors)

mod.0 <- lm(exam.y ~ 1)

# Test the null hypothesis that all of the coefficient vectors
# are zero (that is, none of the predictors have a linear effect
# on either response variable)

anova(mod.all, mod.0)

# Fit a linear model with only predictors 1 and 3 (leaving out 
# predictor 2, 'HomeworkPct')

mod.2 <- lm(exam.y ~ exam.x[,-2])

# Test the null hypothesis that predictor 2, 'HomeworkPct',
# does not have a linear effect on either response variable

anova(mod.all, mod.2)




