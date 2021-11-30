#'kmeans_cluster
#'
#'Perform k-means clustering on a data matrix.
#'
#'@param X numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
#'@param center either the number of clusters, say \(k\), or a set of initial (distinct) cluster centres. If a number, a random center will be assigned.
#'@param max.iter the maximum number of iterations allowed.
#'@param tol Parameters for determining convergence.
#'
#'@return  Contains cell cluster labels for each cell in x and the center for each cluster
#'
#'@examples
#'
#'
#'@export
#

kmeans_cluster <- function(X, center, max.iter, tol){
  if(is.null(dim(X))){
    return(.kmeans_cluster_single(X, center, max.iter, tol))
  }else{
    X <- data.matrix(X)
    return(NULL)
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

