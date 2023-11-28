# ST 558: Multivariate Analytics
# Module 8 R Activity 
# Nora Quick

###########
# Step 2  #
###########

library(mclust)

# Question 1: 
# -----------
auto82 <- read.csv('Auto82MPGData.csv')
head(auto82)

# Question 2: 
# -----------
pairs(auto82[,1:6])


###########
# Step 3  #
###########

# Question 3: 
# -----------
auto82.distEuc <- dist(auto82[,1:6])

auto82.hcEuc <- hclust(auto82.distEuc, method="complete")
auto82.haEuc <- hclust(auto82.distEuc, method="average")
auto82.hsEuc <- hclust(auto82.distEuc, method="single")

names(auto82.hcEuc)

par(mfrow=c(1,3))
plot(auto82.hcEuc, labels=auto82[,7], hang=-1, sub="", main="Complete Linkage")
plot(auto82.haEuc, labels=auto82[,7], hang=-1, sub="", main="Average Linkage")
plot(auto82.hsEuc, labels=auto82[,7], hang=-1, sub="", main="Single Linkage")

# Question 4: 
# -----------
auto82.sc <- scale(auto82[,1:6])
auto82.distEucSc <- dist(auto82.sc)

auto82.hcEucSc <- hclust(auto82.distEucSc, method="complete")
auto82.haEucSc <- hclust(auto82.distEucSc, method="average")
auto82.hsEucSc <- hclust(auto82.distEucSc, method="single")

par(mfrow=c(1,3))
plot(auto82.hcEucSc, labels=auto82[,7], hang=-1)
plot(auto82.haEucSc, labels=auto82[,7], hang=-1)
plot(auto82.hsEucSc, labels=auto82[,7], hang=-1)

# Question 5: 
# -----------
#auto82.hsEucSc <- hclust(auto82.distEucSc, method="single")
#plot(auto82.haEucSc, labels=auto82[,7], hang=-1)

# Question 6: 
# -----------
cutree(auto82.hcEuc, k=4)

pairs(auto82[,1:6], col=cutree(auto82.hcEuc,k=4)+1)



###########
# Step 4  #
###########

# Question 7: 
# -----------
auto82.km3 <- kmeans(auto82[,1:6], centers=4, nstart=10)

names(auto82.km3)

auto82[auto82.km3$clus==1,7]
auto82[auto82.km3$clus==2,7]
auto82[auto82.km3$clus==3,7]
auto82[auto82.km3$clus==4,7]

# Question 8: 
# -----------
table(auto82.km3$clus, cutree(auto82.hcEuc, k=4))

# Question 9: 
# -----------
pairs(auto82[,1:6], col=auto82.km3$clus+1)


###########
# Step 5  #
###########

# Question 11: 
auto82.mc <- Mclust(auto82[,1:6])
auto82.mc

names(auto82.mc)

pairs(auto82[,1:6], col=auto82.mc$clas+1)


# Question 12: 
# -----------
auto82.mc4 <- Mclust(auto82[,1:6], G=4)

pairs(auto82[,1:6], col=auto82.mc4$clas+1)

table(auto82.mc4$clas, auto82.km3$clus)


