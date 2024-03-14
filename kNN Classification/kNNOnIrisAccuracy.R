head(iris)
col = c('blue', 'red', 'green')
plot(Petal.Width ~ Sepal.Width, data = iris, col = col[unclass(iris$Species)])

# Define function for kNN
knn.classify = function(test.x, train.x, train.y, k) {
  n = nrow(train.x)
  p = ncol(train.x)
  stopifnot(length(train.y) == n, length(test.x) == p)  # Verification
  d = rep(0, n)
  for (i in 1:n)
    d[i] = sqrt(sum((test.x - train.x[i, ])^2))  # Distance
  o = order(d)
  nn = o[1:k]
  t = table(train.y[nn])
  return(names(which.max(t)))
}

# Define the number of neighbors
k = 11

# Define the features and response variables
train.x = iris[, c('Sepal.Width', 'Petal.Width')]
train.y = iris$Species

# Define the number of test points
n_test = nrow(iris)


for (i in 1:n_test) {
  test_point = iris[i, c('Sepal.Width', 'Petal.Width')]
  n_test[i] =knn.classify(test_point, train.x, train.y, k)
}

# Calculate the accuracy
actual_classes =iris$Species
accuracy = (sum(predicted_classes == actual_classes) / n_test)*100

# Print the accuracy
cat("Accuracy of kNN model on the Iris dataset:", accuracy, "%\n")



