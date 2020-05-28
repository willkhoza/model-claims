require(dplyr)

# Calculating the goodness of fit 

# generic Poisson probability distribution
p <- function(x, lambda){
  lambda^x*exp(-lambda)/factorial(x)
}
p <- Vectorize(p)

# set lambda = 0.3
p.lambda <- function(x){
  p(x, 0.3)
}

# capture the claim experience
claim.experience <- c(96632, 28648, 4400, 476, 36, 8)

# the short way
x <- 0:(length(claim.experience)-1)
test <- chisq.test(claim.experience, p = p.lambda(x)/sum(p.lambda(x)))


##### SOLUTION #############333333333333333333333333333333333333333
print(test)

##### END OF SOLUTION ######333333333333333333333333333333333333333

# the long way
x <- 0:10
N <- 130200

dat <- cbind(x,
             append(claim.experience, rep(0, length(x) - length(claim.experience))),
             round(N*p.lambda(x), 1))

dat <- data.frame(dat)
colnames(dat) <- c('x', 'actual', 'expected')



dat <- dat %>%
  mutate(z=(actual-expected)/sqrt(expected)) %>%
  filter(expected!=0) %>%
  mutate(chi_2=z^2)

chi_2.sum <- sum(dat$chi_2) 
df <- length(dat$actual) - 1

qchisq(0.025, 5)
qchisq(0.975, 5)

chi.values <- function(quant){
  qchisq(quant, df) 
}
chi.values <- Vectorize(chi.values)

q <- seq(0, 1, length.out =  100)
plot(q~chi.values(q), type = 'l')
abline(v = c(chi.values(0.025), chi.values(0.975)), col = 'green')
abline(h = c(0.025, 0.975), lty=c(2,2))
abline(v = chi_2.sum)



