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

kmeans_cluster = function(X, center, max.iter, tol) {
    check.tol = function(fmax,fmin,ftol){
      delta <- abs(fmax - fmin)
      accuracy <- (abs(fmax) + abs(fmin))*ftol
      return(delta < (accuracy + tol))
    }
    X <- data.matrix(X)
    N <- nrow(X)
    p <- ncol(X)
    nclust <- NULL
    if(length(unique(center)) != 1){
      if(is.null(dim(center))){
        nclust <- length(center)
      }else{
        nclust <- ncol(center)
      }
      Cluster_index <- NULL
      mu <- center
    }else{
      Cluster_index <- sample(1:nclust,N,replace = TRUE)
      mu <- sapply(1:nclust,FUN = function(i) colMeans(X[Cluster_index == i,]))
    }
    SumofSquare_Loss <- 10^10
    for(i in 1:max.iter){
      SumofSquare_Loss0 <- SumofSquare_Loss
      SumOfSquare_diff <- apply(mu, MARGIN = 2,FUN = function(i) rowSums((sweep(X,MARGIN = 2,FUN = "-",i))^2))
      Cluster_index <- apply(SumOfSquare_diff,MARGIN = 1,FUN = function(i) which.min(i))
      SumofSquare_Loss <- sum(t(SumOfSquare_diff)[Cluster_index + seq(0,N * nclust - nclust,nclust)])/N
      mu <- sapply(1:nclust,FUN = function(i) colMeans(X[Cluster_index == i,]))
      if(check.tol(SumofSquare_Loss0,SumofSquare_Loss,tol)){
        break
      }
    }
    return(list(mus <- mu,est <- Cluster_index))
}
