library(expm)  # for matrix powers

# Calculating the expected discount over t = 13 years 

S <- c(0, 25, 50, 60) # set the states

# generic Poisson probability distribution
p <- function(x, lambda){
  lambda^x*exp(-lambda)/factorial(x)
}

# set lambda = 0.3
p.lambda <- function(x){
  p(x, 0.3)
}
p <- Vectorize(p)

# define the transition probability matrix
P <- rbind(c(1-p.lambda(0), p.lambda(0), 0, 0), 
           c(1-p.lambda(0), 0, p.lambda(0), 0), 
           c(1-p.lambda(0)-p.lambda(1), p.lambda(1), 0, p.lambda(0)),
           c(0, 1-p.lambda(0)-p.lambda(1), p.lambda(1), p.lambda(0)))

rowSums(P) # check if the matrix is stochastic

P_0 <- c(1, 0, 0, 0) # initial marginal distribution (t = 0)

sum(P_0) # check if marginal is stochastic

P_ <- function(n){
  P_0 %*% (P%^%n)
}

P_13 <- P_(13) # marginal distribution at t = 13

# solution
E_S_13 <- sum(P_13*S)

##### SOLUTION #############333333333333333333333333333333333333333
print(E_S_13)

##### END OF SOLUTION ######333333333333333333333333333333333333333

