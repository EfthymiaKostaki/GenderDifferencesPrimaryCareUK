get_beta_hat <- function(X) {
  # Note: Don't forget to add  the intercept! using
  # X = cbind(Intercept=rep(1,5), X1, X2); X
  # OLS estimator in matrix notation: beta.hat  = (X'X)ˆ{-1} X'Y
  beta.hat = inv(t(X)%*%X)%*% t(X)%*%Y # %*% matrix multiplication
  return(beta.hat)
}

get_y_hat<- function(X, beta.hat) {
  return(X%*%beta.hat)
}

# Get residual sum of squares
get_rss<- function(y, y_hat) {
  res<- y-y_hat
  return(t(res)%*%res)
}

get_h_matrix<- function(X) {
  hmat<-X%*%(inv(t(X)%*%X))%*% t(X)
  return(hmat)
}

# Omitted variable Bias 
# Get size of the bias
