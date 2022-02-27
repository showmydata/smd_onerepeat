# Functions on this page were taken from:
# Cousineau, D., & Goulet-Pelletier, J. C. A study of confidence intervals for 
# Cohen’s dp in within-subject designs with new proposals.

# Correction factor
J <- function(df) {
  # compute unbiasing factor; works for small or large df;
  # thanks to Robert Calin-Jageman
  exp ( lgamma(df/2) - log(sqrt(df/2)) - lgamma((df-1)/2) )
}

dp <- function(X) {

# Get descriptive statistics
n <- dim(X)[1]
Mx <- mean(X[,1])
My <- mean(X[,2])
sx <- sd(X[,1])
sy <- sd(X[,2])
r <- cor(X[,1], X[,2])
# Get pairwise statistics Delta means and pooled SD
dmn <- Mx-My
sdp <- sqrt((sx^2 + sy^2)/2)
# Compute biased Cohen’s d
dp <- dmn / sdp
dp

}

adjustedlambdaprime <- function(dp, X, gamma = .95) {
  
  # Get descriptive statistics
  n <- dim(X)[1]
  Mx <- mean(X[,1])
  My <- mean(X[,2])
  sx <- sd(X[,1])
  sy <- sd(X[,2])
  r <- cor(X[,1], X[,2])
  
  # Do calculations
  W <- geometric.mean(c(sx^2, sy^2)) / mean(c(sx^2, sy^2))
  rW <- r * W
  lambda <- dp * J(n-1) * sqrt(n/(2*(1-rW)))
  #quantile of the noncentral t distribution
  dlow = qlambdap(1/2-gamma/2, df = 2/(1+r^2)*(n-1), t = lambda )
  dhig = qlambdap(1/2+gamma/2, df = 2/(1+r^2)*(n-1), t = lambda )
  limits <- c(dlow, dhig) / sqrt(n/(2*(1-rW))) / J( 2/(1+r^2)*(n-1) )
  limits
}