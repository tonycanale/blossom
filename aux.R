library(MCMCpack)
library(splines2)
library(tmvtnorm)
# log-densità di una Diriclet
logddirichlet <- function(x, alpha)
  sum(lgamma(alpha)) - lgamma(sum(alpha)) + sum((alpha-1)*log(x))

# logverosimiglianza per la singola i-sima osservazione
loglik_i <- function(i, y, s, t, x, pi, beta, gamma, tau)
{
fsi <- function(x) iSpline(x, knots=knots, Boundary.knots=bounds) %*% pi[,s[i]]
media <- fsi( as.double((t[i]-x[i,]%*%beta)/(x[i,]%*%gamma)))
dnorm(y[i], media, 1/sqrt(tau), log=TRUE)
}
# logverosimiglianza per tutti
loglik <- function(y, s, t, x, pi, beta, gamma, tau) 
  sum(sapply(1:length(y),loglik_i, y=y,s=s,t=t,x=x, pi=pi,
             beta=beta, gamma=gamma, tau=tau))

# sample the spline coefficients
sample_theta <- function(i, y, s, t, x, pi, beta, gamma, tau, lambda)
{
  rows <- which(s==i)
  subrow <- !is.na(y[rows])
  B <- B[rows,][subrow,]
  varianza <- ginv(t(B) %*% B +lambda*P)
  media <-  as.double(varianza %*% t(B) %*% y[rows][subrow])
  start <- rtruncnorm(1, a = c(-Inf, rep(0, ncol(B)-1)), b=Inf, mean = media, sd=.1)
  theta_s <- as.double(rtmvnorm(1, lower = c(-Inf, rep(0, ncol(B)-1)), upper=rep(Inf, ncol(B)), 
                                mean = media, sigma=varianza,algo="gibbs", start.value=start), burn.in=10)
theta_s
}
  
theta_s <- sapply(1:S, sample_theta, y,si,t,x, pi, beta, gamma, tau, lambda)
par(mfrow=c(2,3))
for(i in 1:6) 
{
  plot(seq(0,360, length=100),basi %*% theta_s[,i], ty='l', col=i)
  plot(seq(0,360, length=100),basi %*% theta_s[,i], ty='l', col=i)
}

sample_beta <- function(i, y, s, t, x, pi, beta, gamma, tau, lambda)
{
  beta_star <- 
  a1 <- loglik(y[s==i], s[s==i], t[s==i], x[s==i,], pi=matrix(rep(pistar,S), ncol=S), 
               beta=beta, gamma=gamma, tau=tau) -
    loglik(y[s==i], s[s==i], t[s==i], x[s==i,], pi=pi, 
           beta=beta, gamma=gamma, tau=tau) # prior non informativa
  a2 <- 
  a <- exp(a1+a2)
  a <- ifelse(a>1,1,a)
  keep <- sample(c(1,0),1,prob=c(a,1-a))
  if(keep){ return(pistar)}
  else{ return(pi[,i])}
}




### OLD SUFF 

# campiona i pesi della specie i
sample_pi <- function(i, y, s, t, x, pi, beta, gamma, tau)
{
  pistar <- as.double(rdirichlet(1, 100*pi[,i]))
  pistar[pistar==0] <- .Machine$double.eps
  pistar <- pistar/sum(pistar)
  plot(pistar, pi[,i])
  abline(0,1)
  a1 <- loglik(y[s==i], s[s==i], t[s==i], x[s==i,], pi=matrix(rep(pistar,S), ncol=S), 
               beta=beta, gamma=gamma, tau=tau) -
    loglik(y[s==i], s[s==i], t[s==i], x[s==i,], pi=pi, 
           beta=beta, gamma=gamma, tau=tau) # prior non informativa
  a2 <- logddirichlet(pi[,i], 100*pistar) -  logddirichlet(pistar, 100*pi[,i])  
  a <- exp(a1+a2)
  a <- ifelse(a>1,1,a)
  keep <- sample(c(1,0),1,prob=c(a,1-a))
  if(keep){ return(pistar)}
  else{ return(pi[,i])}
}
#sample_pi(1, y,si,t,x, pi, beta, gamma, tau)
sample_pi(3, y,si,t,x, pi, beta, gamma, tau)

sapply(1:S, sample_pi, y,si,t,x, pi, beta, gamma, tau)