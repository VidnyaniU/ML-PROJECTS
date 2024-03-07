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

knn.classify(test.x,train.x,train.y,k)


