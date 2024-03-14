n=1000
x=seq(0,10,length=n)
y=2+sin(pi*x)+rnorm(n)
plot(x, y, col = "yellow", main = "Running Mean", xlab = "x", ylab = "y")
curve(2+sin(pi*x),min(x),max(x),1000,add=TRUE,col="red",lwd=3)
#plot(x,y,col="yellow")
bins=0:10
m=11
abline(v=bins,lty=2)

##Running mean
#instead of taking bins for mean we take a window of some specific width

w = 0.05  # Window width
y.hat = rep(0, n)

for (i in 1:n) {
  b = x[i] + w * c(-1,+1)
  y.hat[i] = mean(y[which((b[1] <= x) & (x <= b[2]))])
}

for (i in 1:(m - 1)) {
  lines(bins[i:(i+1)], rep(y.hat[i], 2), col = "blue")
}