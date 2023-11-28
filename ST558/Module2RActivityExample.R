# ST 558: Multivariate Analytics
# Module 2 R Activity Example Script File

###########
# Step 2  #
###########

rslt1 <- 7
rslt2 = rslt1
rslt2

rslt2 == rslt1

rslt3 <- 10
rslt3 == rslt2

###########
# Step 3  #
###########

vec1 <- c(3, 2, 10, -1)
vec1

length(vec1)

dim(vec1)

vec2 <- c(3, -2, 10, -1)
vec2 == vec1

all(vec2 == vec1)

###########
# Step 4  #
###########

mat1 <- cbind(c(3,-1,-2), c(8,1,5))
mat1

mat2 <- rbind(c(3,-1,-2), c(8,1,5))
mat2

mat3 <- matrix(c(3,-1,-2,8,1,5), nrow=3, ncol=2)
mat3

mat4 <- matrix(c(3,-1,-2,8,1,5), nrow=3, ncol=2, byrow=TRUE)
mat4

dim(mat1)
dim(mat2)

mat1 == mat2

mat3 == mat4

all(mat3 == mat4)

###########
# Step 5  #
###########

mat1
t(mat1)

mat2
t(mat1) == mat2
all(t(mat1) == mat2)

###########
# Step 6  #
###########

mat6 <- cbind(c(3, -1, -2), c(-3, 1, 5), c(-8, 4, 7))
mat6
diag(mat6)

mat7 <- diag(c(3,1,7))
mat7

mat8 <- diag(4)
mat8

###########
# Step 7  #
###########

matA <- cbind(c(3,-1,-2), c(8,1,5))
matB <- cbind(c(-2,7,-3), c(4,3,6))

matA
matB
matA + matB

matC <- cbind(matB, c(0,8,11))

matC

matA + matC

###########
# Step 8  #
###########

matA <- cbind(c(3,-1,-2), c(8,1,5))
constC <- 4

matA

constC * matA

10*matA

###########
# Step 9  #
###########

matA <- cbind(c(2,3), c(4, 5))
matB <- cbind(c(1,7), c(10, 6))

matA
matB
matA * matB

matA %*% matB

matA <- rbind(c(1, 2, 3), c(7, 2, -4))
matB <- cbind(c(1, 2, 8), c(0, 6, 2))

matA
matB
matA %*% matB
matB %*% matA

matA %*% t(matB)

groceryPrices <- rbind(c(0.4, 3.5, 0.5), c(0.35, 3.3, 0.55), c(0.38, 3.25, 0.45), c(0.33, 3.1, 0.6), c(0.42, 3.4, 0.55))

recipeAmounts <- c(1, 0.5, 0.25)

groceryPrices %*% recipeAmounts

###########
# Step 10 #
###########

matA <- cbind(c(1,0.5), c(3,0.5))
matA

matB <- matA^(-1)
matB

matB %*% matA

matA.inv <- solve(matA)
matA.inv

matA.inv %*% matA

###########
# Step 11 #
###########

matA

det(matA)

sum(diag(matA))

###########
# Step 12 #
###########

matA <- cbind(c(6.1, -6.3), c(-6.3, 22.9))
matA

matA.eig <- eigen(matA)
matA.eig

matA.eig$vec

matA.eig$val

matV <- matA.eig$vec
matLam <- diag(matA.eig$val)
matLam

matV %*% matLam %*% t(matV)
matA

matA %*% matV[,1]
matV[,1]
matV[,1]*25

det(matA)
prod(matA.eig$val)

sum(diag(matA))
sum(matA.eig$val)

