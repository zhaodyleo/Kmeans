test_that("Error check works", {
  expect_error(kmeans_cluster(c(1,6,3,2,6),2.2))
  expect_error(kmeans_cluster(c(1,6,3,2,6),c(2.2,2.2)))
  expect_error(kmeans_cluster(c(1,6,3,2,6),1))
})

X1 = runif(100,0,1)
X2 = runif(100,2,3)
index = sample(1:200)
train_x = c(X1,X2)[index]
train_label = c(rep(0,100),rep(1,100))[index]

test_that("accuracy check", {
  expect_equal(kmeans_label_rematch(kmeans_cluster(train_x,2)[[2]],train_label)[[2]],1)
  expect_equal(kmeans_label_rematch(kmeans_cluster(train_x,c(0,3))[[2]],train_label)[[2]],1)
})

