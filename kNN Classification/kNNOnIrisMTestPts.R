
head(iris)
col = c('blue','red','green')
plot(Petal.Width~Sepal.Width,data=iris,col=col[unclass(iris$Species)])

#train and test 
train.x = iris[,c('Sepal.Width','Petal.Width')]
train.y = iris$Species
test.x =c(3.5,0.2)
k=7 #kNN
##################
#function for kNN#
##################
knn.classify = function(test.x , train.x,train.y , k){
  n = nrow(train.x)
  p=ncol(train.x)
  stopifnot(length(train.y)==n, length(test.x)==p)#verification
  d=rep(0,n)
  for(i in 1:n)
    d[i]=sqrt(sum((test.x-train.x[i,])^2))#distance
  o = order(d)
  nn=o[1:k]
  #train.y[nn]
  t=table(train.y[nn])
  return (names(which.max(t)))
}

# number of test points
m = 11

# grid of test points
sw.grid = seq(min(iris$Sepal.Width), max(iris$Sepal.Width), length = m)
pw.grid = seq(min(iris$Petal.Width), max(iris$Petal.Width), length = m)

# matrix to store the predicted classes
predicted_classes = matrix(NA, nrow = length(sw.grid), ncol = length(pw.grid))
# classify each point in the grid
# there will be 121 points if m =11

for (i in 1:length(sw.grid)) {
  for (j in 1:length(pw.grid)) {
    test_point = c(sw.grid[i], pw.grid[j])
    predicted_classes[i, j] = knn.classify(test_point, train.x, train.y, k)
  }
}
# Convert predicted classes to a data frame 
predicted_df = expand.grid(Sepal.Width = sw.grid, Petal.Width = pw.grid)
predicted_df$Species = as.factor(as.vector(predicted_classes))

head(predicted_df)
col=c('magenta','yellow','cyan')
plot(Petal.Width~Sepal.Width,data=predicted_df,col=col[unclass(predicted_df$Species)])


