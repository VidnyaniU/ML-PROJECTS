n=1000
x=seq(0,10,length=n)
y=2+sin(pi*x)+rnorm(n)
plot(x, y, col = "yellow", main = "Quantile Regression", xlab = "x", ylab = "y")
curve(2+sin(pi*x),min(x),max(x),1000,add=TRUE,col="red",lwd=3)
#plot(x,y,col="yellow")
bins=0:10
m=11
abline(v=bins,lty=2)

##Quantile regression
#instead of mean we take boxplot for quantile regression
y.hat = rep(0, n)

for (i in 1:n) {
    y.hat[i] = boxplot(y[which((bins[i] <= x) & (x <= bins[i+1]))] , at=i,add=TRUE,outline =FALSE)
}

