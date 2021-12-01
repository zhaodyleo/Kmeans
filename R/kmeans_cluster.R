#'kmeans_cluster
#'
#'Perform k-means clustering on a data matrix.
#'
#'@param X numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
#'@param center either the number of clusters, say \(k\), or a set of initial (distinct) cluster centres. If a number, a random center will be assigned.
#'@param max.iter the maximum number of iterations allowed.
#'@param tol Parameters for determining convergence.
#'
#'@return a list contains centres of each cluster and the labels for each observation
#'
#'@examples
#'
#'# One Dimension Example
#'x1 = runif(10000,0,1)
#'x2 = runif(10000,2,3)
#'x3 = runif(10000,4,5)
#'train_data = c(x1,x2,x3)
#'train_label = c(rep(0,10000),rep(1,10000),rep(2,10000))
#'training_result = kmeans_cluster(train_data,3,2500,1e-8)
#'
#'# Multi Dimension Example
#'X1 = runif(10000,0,1)
#'X2 = runif(10000,0,1)
#'X3 = runif(10000,3,5)
#'X4 = runif(10000,3,5)
#'train_data = cbind(c(X1,X3),c(X2,X4))
#'train_label = c(rep(0,10000),rep(1,10000))
#'training_result = kmeans_cluster(train_data,3,2500,1e-8)
#'@export
#

kmeans_cluster <- function(X, center, max.iter = 2500, tol = 1e-8){
  if(is.null(dim(X))){
    return(.kmeans_cluster_single(X, center, max.iter, tol))
  }else{
    X <- data.matrix(X)
    return(.kmeans_cluster_multi(X,center,max.iter,tol))
  }
}

.check.tol <- function(fmax,fmin,ftol){
  delta <- abs(fmax - fmin)
  accuracy <- (abs(fmax) + abs(fmin))*ftol
  return(delta < (accuracy + ftol))
}

.kmeans_cluster_single <- function(X, center, max.iter, tol) {
  nclust <- NULL
  # check whether randomly assigned initial centres or use the input one
  # initialized the centres and label
  if(length(unique(center)) != 1){
    stopifnot("Center should include distinct value" = length(unique(center)) == length(center))
    nclust <- length(center)
    Cluster_index <- NULL
    mu <- center
  }else{
    stopifnot("Center includes unique value but length not equal to 1" = length(center) == 1)
    stopifnot("Center must be integer" = as.integer(center) == center)
    stopifnot("Center should be greater than 1" = center > 1)
    nclust <- center
    Cluster_index <- sample(1:nclust,length(X),replace = TRUE)
    mu <- sapply(1:nclust,
                 FUN = function(i) mean(X[Cluster_index == i]))
  }
  # initialized the loss
  SumofSquare_Loss <- 10^10
  for(i in 1:max.iter){
    SumofSquare_Loss0 <- SumofSquare_Loss
    # apply the E step of algorithm which update
    # sum of square loss, labels
    SumOfSquare_diff <- sapply(mu,
                              FUN = function(i) (X-i)^2)
    Cluster_index <- apply(SumOfSquare_diff,
                           MARGIN = 1,
                           FUN = function(i) which.min(i))
    SumofSquare_Loss <- sum(t(SumOfSquare_diff)[Cluster_index + seq(0,length(X) * nclust - nclust,nclust)])/length(X)
    # apply the M step of algorithm which update the centres
    mu <- sapply(1:nclust,
                 FUN = function(i) mean(X[Cluster_index == i]))
    # check whether loss converge
    if(.check.tol(SumofSquare_Loss0,SumofSquare_Loss,tol)){
      break
    }
  }
  return(list(mus <- mu,est <- Cluster_index))
}

.kmeans_cluster_multi <- function(X, center, max.iter, tol) {
  N <- nrow(X)
  p <- ncol(X)
  nclust <- NULL
  # check whether randomly assigned initial centres or use the input one
  if(is.null(dim(center))){
    stopifnot("You tried to assign number of cluster but not input a valid integer" = as.integer(center) == center)
    stopifnot("Center should be greater than 1" = center > 1)
    nclust <- center
  }else{
    stopifnot("the row dimension of center is not match the column dimension of X" = dim(X)[2] == dim(center)[1])
    nclust <- ncol(center)
  }
  # initialized the centres and label
  if(length(unique(center)) != 1){
    Cluster_index <- NULL
    mu <- center
  }else{
    Cluster_index <- sample(1:nclust,N,replace = TRUE)
    mu <- sapply(1:nclust,
                 FUN = function(i) colMeans(X[Cluster_index == i,]))
  }
  # initialized the loss
  SumofSquare_Loss <- 10^10
  for(i in 1:max.iter){
    SumofSquare_Loss0 <- SumofSquare_Loss
    # apply the E step of algorithm which update
    # sum of square loss, labels
    SumOfSquare_diff <- apply(mu,
                              MARGIN = 2,
                              FUN = function(i) rowSums((sweep(X,MARGIN = 2,FUN = "-",i))^2))
    Cluster_index <- apply(SumOfSquare_diff,
                           MARGIN = 1,
                           FUN = function(i) which.min(i))
    SumofSquare_Loss <- sum(t(SumOfSquare_diff)[Cluster_index + seq(0,N * nclust - nclust,nclust)])/N
    # apply the M step of algorithm which update the centres
    mu <- sapply(1:nclust,
                 FUN = function(i) colMeans(X[Cluster_index == i,]))
    # check whether loss converge
    if(check.tol(SumofSquare_Loss0,SumofSquare_Loss,tol)){
      break
    }
  }
  return(list(mus <- mu,est <- Cluster_index))
}




