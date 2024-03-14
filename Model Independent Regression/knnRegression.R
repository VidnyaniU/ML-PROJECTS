n=50
x1=runif(n)
x2=runif(n)

y=x1^2+x2^2+rnorm(50,sd=0.2)
plot(x1,x2,main="kNN Regression")

x1.test = x2.test=0.5
points(x1.test,x2.test,col="red",pch =19)

k=5
d=NULL
#distances
for(i in 1:n){
  d[i]=sqrt((x1.test-x1[i])^2+(x2.test-x2[i])^2)
}

o=order(d)
d[o]
kInd=o[1:k] #indices of first k nearest neighbours
x1[kInd]
x2[kInd]
points(x1[kInd],x2[kInd],col="green",pch=19)
mean(y[kInd])
