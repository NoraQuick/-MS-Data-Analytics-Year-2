# ST 558: Multivariate Analytics
# Module 2 R Activity: Nora Quick

# 2. Assignment operator vs. checking for equality
rslt1 <- 7
rslt2 = rslt1
rslt2

rslt2 == rslt1

rslt3 <- 10
rslt3 == rslt2

# Question 1: 
# -----------
newVec1 <- c(1, 4, 9, 12)
newVec1

# Question 2:
# -----------
newVec2 <- (1:4)^2 
newVec2

# Question 3:
# -----------
newMat1 <- cbind(c(1, 4, 2), c(0, 7, -3), c(5, 2, 1))
newMat1

# Question 4:
# -----------
newMat2 <- rbind(c(1, 1, 2), c(4, 7, 2), c(5, -3, -1))
newMat2

# Question 5:
# -----------
newMat1 == newMat2

# Question 6:
# -----------
newMat3 <- matrix(c(1:6), nrow=3, ncol=2)
newMat3

# Question 7
# -----------
t(newMat2) == newMat1

# Question 8
# ----------
isSymmetric(newMat1)

# Question 9
# ----------
diag(c(1, 7, 1))

# Question 10
# -----------
ident5 <- diag(5)
ident5

# Question 11
# -----------
newMat1 + newMat2

# Question 12
# -----------
7*newMat3

# Question 13
# -----------
ident3 <- diag(3)
ident3

newMat3 <- ident3 %*% ident3
newMat3

# Question 14
# -----------
newMat3 <- ident3 %*% 3
newMat3

# Question 15
# -----------
newMat1.inv <- solve(newMat1)
newMat1.inv

# Question 16
# -----------
newMat1 %*% newMat1.inv
newMat1.inv %*% newMat1

# Question 17
# -----------
notSquare <- matrix(c(1:6), nrow=3, ncol=2)
det(notSquare)

# Question 18
# -----------
sum(diag(notSquare))

# Question 19
# -----------
det(newMat2)

# Question 20
# -----------
sum(diag(newMat2))

# Question 21
# -----------
newMat2 <- cbind(c(6.8, 2.4, 0), c(2.4, 8.2, 0), c(0, 0, 0.5))
newMat2

newMat2.eig <- eigen(newMat2)
newMat2.eig

newMat2.eig$vec
newMat2.eig$val

newMat2V <- newMat2.eig$vec
newMat2Lam <- diag(newMat2.eig$val)

newMat2V %*% newMat2Lam %*% t(newMat2V)

matA %*% matV[,1]
matV[,1]
matV[,1]*25

det(matA)
prod(matA.eig$val)

sum(diag(matA))
sum(matA.eig$val)










