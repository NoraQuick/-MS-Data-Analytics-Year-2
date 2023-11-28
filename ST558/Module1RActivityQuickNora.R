# ST 558: Multivariate Analytics
# Module 1 R Activity: Nora Quick

# Set working directory 
setwd('/home/quickn')

# 2. Read in the ‘SeedsData.csv’ dataset from the .csv file posted under Module 1
seedsdata <- read.csv('SeedsData.csv')

# Question 1: 
# -----------
# Dimensions
dim(seedsdata)

# Question 2:
# -----------
# Head and tail of dataset
head(seedsdata)
tail(seedsdata)

# Question 3:
# -----------
# Summary of the columns in a data frame
summary(seedsdata)

# Question 4:
# -----------
# Structure of a dataset
str(seedsdata)

# Question 5:
# -----------
# Convert 'Variety' variable to factor
variety.orig <- seedsdata$Variety
seedsdata$Variety <- factor(variety.orig, levels=sort(unique(variety.orig)), ordered=T)
str(seedsdata)

# Now, revert 'Variety' to its original format.
seedsdata$Variety <- variety.orig

# Question 6:
# -----------
# Make histograms
par(mfrow=c(2,4))
seedsdata[,1:7] 

#histogram
for(j in 1:7){
  hist(seedsdata[,j], xlab=names(seedsdata)[j], main="")
}

# Question 7
# -----------
# Pariwise scatterplot
pairs(seedsdata[,1:7], col=rainbow(13)[factor(seedsdata$Variety)])

# Question 8
# ----------
# Sample Mean Vectors
seedsdata.samplemean <- apply(seedsdata[,1:8], 2, mean)
seedsdata.samplemean

# Question 9
# ----------
# Sample Covariance Matrix
seedsdata.samplecov <- cov(seedsdata[,1:8], use='pairwise.complete.obs')
seedsdata.samplecov

# Question 10
# -----------
# Sample Correlation Matrix
seedsdata.samplecor <- cor(seedsdata[,1:8], use='pairwise.complete.obs')
seedsdata.samplecor

# Questions (11 - 14)
# -----------------
# Multiply Compactness by 100
seeds.mod1 <- seedsdata
seeds.mod1$Compactness <- seeds.mod1$Compactness * 100

# Question 11
# -----------
# New sample mean vector
apply(seeds.mod1[,1:8], 2, mean, na.rm=T)

# Question 12
# -----------
# New sample covariance matrix
cov(seeds.mod1[,1:8], use='pairwise.complete.obs')

# Question 13
# -----------
# New sample correlation matrix
cor(seeds.mod1[,1:8], use='pairwise.complete.obs')

# Question 14
# -----------
# NA

# Questions 15 - 18
# -----------------
# Subtract Asymmetry by sample mean
seeds.mod2 <- seedsdata
seeds.mod2$Asymmetry <- seeds.mod2$Asymmetry - mean(seeds.mod2$Asymmetry, na.rm=T)

# Question 15
# -----------
# New sample mean vector 
apply(seeds.mod2[,1:8], 2, mean, na.rm=T)

# Question 16
# -----------
# New covariance matrix
cov(seeds.mod2[,1:8], use='pairwise.complete.obs')

# Question 17
# -----------
# New correlation matrix
cor(seeds.mod2[,1:8], use='pairwise.complete.obs')




