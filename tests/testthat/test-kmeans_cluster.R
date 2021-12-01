test_that("Error check works", {
  expect_error(kmeans_cluster(c(1,6,3,2,6),2.2))
  expect_error(kmeans_cluster(c(1,6,3,2,6),c(2.2,2.2)))
  expect_error(kmeans_cluster(c(1,6,3,2,6),1))
})


test_that("accuracy check for single", {
  X1 <- runif(100,0,1)
  X2 <- runif(100,2,3)
  index <- sample(1:200)
  train_x <- c(X1,X2)[index]
  train_label <- c(rep(0,100),rep(1,100))[index]
  expect_equal(kmeans_label_rematch(kmeans_cluster(train_x,2)[[2]],train_label)[[2]],1)
  expect_equal(kmeans_label_rematch(kmeans_cluster(train_x,c(0,3))[[2]],train_label)[[2]],1)
})



test_that("accuracy check for multi case", {
  X1 <- runif(1000,0,1)
  X2 <- runif(1000,0,1)
  X3 <- runif(1000,2,3)
  X4 <- runif(1000,2,3)
  index <- sample(1:2000)
  train_x_multi <- cbind(c(X1,X3),c(X2,X4))[index,]
  train_label_multi <- c(rep(0,1000),rep(1,1000))[index]

  expect_equal(kmeans_label_rematch(kmeans_cluster(train_x_multi,2)[[2]],train_label_multi)[[2]],1)
  expect_equal(kmeans_label_rematch(kmeans_cluster(train_x_multi,matrix(c(0,0,2,2),2,2))[[2]],train_label_multi)[[2]],1)
})
