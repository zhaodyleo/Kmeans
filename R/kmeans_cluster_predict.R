#'kmeans_cluster_predict
#'
#'predict the label for each new data it belongs
#'
#'@param X numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
#'@param center the centres for each clusters
#'
#'@return  label for each row observations
#'
#'@examples
#'    x = c(1,2,4,6,3)
#'    mu = c(2,4)
#'    kmeans_cluster_predict(x, mu)
#'@export
#

kmeans_cluster_predict <- function(X, center){
  if(is.null(dim(X))){
    SumOfSquare_diff <- sapply(mu,
                               FUN = function(i) (X-i)^2)
    Cluster_index <- apply(SumOfSquare_diff,
                           MARGIN = 1,
                           FUN = function(i) which.min(i))
    return(Cluster_index)
  }else{
    X <- data.matrix(X)
    SumOfSquare_diff <- apply(center,
                              MARGIN = 2,
                              FUN = function(i) rowSums((sweep(X,MARGIN = 2,FUN = "-",i))^2))
    Cluster_index <- apply(SumOfSquare_diff,
                           MARGIN = 1,
                           FUN = function(i) which.min(i))
    return(Cluster_index)
  }
}
