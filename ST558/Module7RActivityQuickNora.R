# ST 558: Multivariate Analytics
# Module 7 R Activity 
# Nora Quick

###########
# Step 2  #
###########

# Question 1: 
# -----------
seeds <- read.csv('Seeds3Data.csv')
head(seeds)

# Question 2: 
# -----------
pairs(seeds[,1:7])


###########
# Step 3  #
###########

# Question 3: 
# -----------
seeds.cor <- cor(seeds)

seeds.eig <- eigen(seeds.cor)
seeds.eig

seeds.pcfa <- seeds.eig$vec %*% diag(sqrt(seeds.eig$val))
seeds.pcfa

# Question 4: 
# -----------
seeds.eig$vec[,1] * sqrt(seeds.eig$val[1])
seeds.eig$vec[,2] * sqrt(seeds.eig$val[2])
seeds.eig$vec[,3] * sqrt(seeds.eig$val[3])

# Question 5: 
# -----------
m <- 3

uni.pcfa3 <- diag(seeds.cor - seeds.pcfa[,1:m] %*% t(seeds.pcfa[,1:m]))
uni.pcfa3

fit.pcfa3 <- seeds.pcfa[,1:m] %*% t(seeds.pcfa[,1:m]) + diag(uni.pcfa3)
fit.pcfa3

res.pcfa3 <- seeds.cor - fit.pcfa3
res.pcfa3


###########
# Step 4  #
###########

# Question 6: 
# -----------
seeds.mlfa1 <- factanal(x=seeds, factors=3, rotation="none")
seeds.mlfa1

# Question 7: 
# -----------
seeds.mlfa1$STATISTIC
seeds.mlfa1$PVAL

# Question 8: 
# -----------
seeds.mlfa1$loadings

# Question 9: 
# -----------
fit.mle3 <- seeds.mlfa1$load %*% t(seeds.mlfa1$load) + diag(seeds.mlfa1$uni)
fit.mle3

res.mle3 <- seeds.cor - fit.mle3
res.mle3


###########
# Step 5  #
###########

# Question 11: 
seeds.mlfa3.rot <- factanal(x=seeds, factors=3, rotation="varimax", scores="regression")
seeds.mlfa3.rot

# Question 12: 
# -----------
seeds.mlfa3.rot$scores[1:10,]
scale(seeds, center=T, scale=F)[1:10,]


