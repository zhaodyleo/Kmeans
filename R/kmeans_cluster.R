#'kmeans_cluster
#'
#'Perform k-means clustering on a data matrix.
#'
#'@param x numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
#'@param nclust the maximum number of iterations allowed.
#'@param max.iter  maximun number of interations
#'@param tol Parameters for determining convergence.
#'
#'@return  Contains cell cluster labels for each cell in x and the center for each cluster
#'
#'@examples
#'square(3)
#'
#'@export
#

kmeans_cluster = function(X, nclust, max.iter, tol) {
    X = data.matrix(X)
    .E_step = function(){
      SumOfSquare_diff <<- apply(mu, MARGIN = 2,FUN = function(i) rowSums((sweep(X,MARGIN = 2,FUN = "-",i))^2))
      Cluster_index <<- apply(SumOfSquare_diff,MARGIN = 1,FUN = function(i) which.min(i))
      SumofSquare_Loss <<- sum(t(SumOfSquare_diff)[Cluster_index + seq(0,N * nclust - nclust,nclust)])/N
    }
    .M_step = function(){
      mu <<- sapply(1:nclust,FUN = function(i) colMeans(X[Cluster_index == i,]))
    }
    check.tol = function(fmax,fmin,ftol){
      delta = abs(fmax - fmin)
      accuracy = (abs(fmax) + abs(fmin))*ftol
      return(delta < (accuracy + tol))
    }
    N <- nrow(X)
    p <- ncol(X)
    Cluster_index <<- rep_len(1:nclust,N)
    mu <<- sapply(1:nclust,FUN = function(i) colMeans(X[Cluster_index == i,]))
    SumofSquare_Loss = 10^10
    for(i in 1:max.iter){
      SumofSquare_Loss0 <- SumofSquare_Loss
      .E_step()
      .M_step()
      if(check.tol(SumofSquare_Loss0,SumofSquare_Loss,tol)){
        break
      }
    }
    return(list(mus=mu,est= Cluster_index))
}
