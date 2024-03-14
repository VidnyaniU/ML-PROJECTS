n=1000
x=seq(0,10,length=n)
y=2+sin(pi*x)+rnorm(n)
plot(x,y,col="yellow")
curve(2+sin(pi*x),min(x),max(x),1000,add=TRUE,col="red",lwd=3)
bins=0:10
m=11
abline(v=bins,lty=2)
###########################
#regressogram

y.hat=rep(0,m-1)
for (i in 1: (m-2)) {
  y.hat[i]=mean(y[which((bins[i]<=x)&(x<bins[i+1]))])
}
#for the last bin 
y.hat[m-1]=mean(y[which((bins[m-1]<=x)&(x<=bins[m]))])

for (i in 1:(m-1)) {
  
  lines(bins[i:(i+1)],rep(y.hat[i],2),col="blue")
}