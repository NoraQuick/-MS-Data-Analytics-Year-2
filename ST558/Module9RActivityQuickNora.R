# ST 558: Multivariate Analytics
# Module 9 R Activity 
# Nora Quick

###########
# Step 2  #
###########

library(ca)
library(psych)
library(mclust)

# Question 1: 
# -----------
milk <- read.csv('MammalMilkData.csv', row.names = 1)
head(milk)

# Question 2: 
# -----------
bdmonth <- read.csv('BirthDeathMonthData.csv', row.names = 1)
head(bdmonth)


###########
# Step 3  #
###########

# Question 3: 
# -----------
milk.distEuc <- dist(milk)
milk.distEuc

milk.loc.mds0 <- cmdscale(milk.distEuc, k=2)
milk.loc.mds0

# Question 4: 
# -----------
plot(milk.loc.mds0, pch=16)
text(milk.loc.mds0, labels=row.names(milk), pos=1, cex=0.5)

milk.loc.mds <- -milk.loc.mds0

plot(milk.loc.mds, pch=16)
text(milk.loc.mds, row.names(milk), pos=1, cex=0.5)


###########
# Step 4  #
###########

# Question 6: 
# -----------
bdmonth.ca <- ca(bdmonth)
bdmonth.ca

plot(bdmonth.ca)

