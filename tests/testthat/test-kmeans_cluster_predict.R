

test_that("predict label check - single", {
  X1 <- c(2,3,12,11,2,1,9)
  X2 <- c(1,1,2,2,1,1,2)
  expect_equal(kmeans_cluster_predict(X1,c(4,8)), X2)
})




test_that("predict label check - Multi", {
  X1 <- runif(1000,0,1)
  X2 <- runif(1000,0,1)
  X3 <- runif(1000,2,3)
  X4 <- runif(1000,2,3)
  index <- sample(1:2000)
  train_data <- cbind(c(X1,X3),c(X2,X4))[index,]
  train_label <- c(rep(0,1000),rep(1,1000))[index]
  result <- kmeans(train_data,2)
  expect_equal(kmeans_cluster_predict(train_data,t(result$centers)), result$cluster)
})

