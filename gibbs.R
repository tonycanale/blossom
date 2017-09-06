# fioriture gibbs
# by tony canale
# giugno 2017

# libraries
library(ggplot2)
library(MASS)
library(truncnorm)
# mcmc parameters
nrep <- 5000
nb <- 1000
ntot <- nrep + nb

# number of species
S <- 6
s.label <- names(table(fioriture$specie))
# locations
L <- 14
# dimension of the predictor
p <- 2

# prior distributions
# beta ~ N(beta0, S0)
beta0 <- rep(0, p)
gamma0 <- rep(1, p)
Sgamma <- Sbeta <- diag(10,p)
atau <- btau <- 1/2
alpha <- rep(1/ncol(fioriture$isplines), ncol(fioriture$isplines))

# arrays to store the MCMC samples
theta <- array(NA, dim=c(ntot, ncol(fioriture$isplines), S))
beta <- matrix(NA, ntot, p)
gamma <- matrix(NA, ntot, p)
tau <- rep(NA, ntot)

# initialization 
beta[1, ] <- 0 
gamma[1, ] <- 1
tau[1] <- 1
D <- cbind(diag(1, ncol(B)-2), 0, 0) + cbind(0,0,diag(1, ncol(B)-2)) + cbind(0,diag(-2, ncol(B)-2),0) 
P <- t(D) %*% D
for(s in 1:S)
{
  rows <- which(fioriture$specie==s.label[s])
  B <- fioriture$isplines[rows,]
  subrow <- !is.na(fioriture$y[rows])
  B <- B[subrow,]
  cbind(1, iSpline(fioriture$t[rows][subrow], knots = knots, 
                   Boundary.knots=bounds))
  prova0 <- ginv(t(B) %*% B + 1*P) %*% t(B) %*% y[rows][subrow]
  prova <- rtruncnorm(1, a = c(-Inf, rep(0, 15)), b=Inf, mean = prova0, sd=.1)
  theta[1,,s] <- prova
  plot(fioriture$t[rows], y[rows], xlim=c(0,360), ylim=c(-10,10))
  points(seq(0,360, length=100),basi %*% prova0, ty='l', col=2)
  points(seq(0,360, length=100),basi %*% prova, ty='l')
}



# start the MCMC
for(i in 1:ntot)
{

  # sample new weights
  
    
}
