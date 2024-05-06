roll = 1:5
marks = c(12,14,17,12,11)
d=data.frame(roll , marks)
#write.table(d,"path.xls",sep ="\t" , row.names = FALSE)
d1=data.frame("x"=roll , "y" = marks)
x1=d1$x
y1=d1$y
n=length(x1)
xbar = sum(x1)/n
varx1 = sum(x1^2)/n - xbar^2
varx1
ybar = sum (y1)/n
covxy= sum(x1*y1)/n - xbar*ybar
covxy

d[3,2]#to access elements from data frame

##subset 
d1
d2=subset(d1,x>2 &  x<5)
d2

##transform
d3=transform(d1,ad=x+y)
d3

###resident data sets in R
d=data(Orange)
Orange
head(Orange)
help(Orange)


data(CO2)
CO2
length(CO2)
CO2$Plant
CO2$Treatment
##functions in R

x=1:5
y=6:10

#distance between  x and y
dxy = sqrt(sum((x-y)^2))
dxy

euclid=function(x,y){
  d=sqrt(sum((x-y)^2))
  
  return (d)
  
}
euclid(x,y)

##loops
##for
x=c()#empty vector
for(i in 1:10){
  x[i] = i
}
n = 1:100
plot(n,1/(n^2) , type="l",col = "red" , lwd=5)

#approximating finite integral
#f(x)=x
intg = function(n){
  grid=seq(from=1 , to=5,by=4/(n-1))#smaller the by value approximation gets better 
  grid
  #n=length(grid)
  #n
  area=0
  for (i in 1:(n-1)) {
    m=(grid[i+1]+grid[i])/2  #midpoint
    area = area+((grid[i+1]-grid[i])*exp(-m))
  }
  return (area)
}
intg(15)
###################

area1=c()
n=15
for(i in 1:n-1){
  area1[i]=intg(i+1 )
  print(i)
}
plot(2:n,area1, type="l",col="blue",lwd= 3)
abline(h=0.36,col="red")



##################plots and curves#################
x=seq(from=-3 , to = 5,by =0.01)
##plot of normal distribution
nrm = function(x) 
{
  m=1
  s=2
  f = (1/(sqrt(2*pi)*s))*exp(-0.5*((x-m)/s)^2)
  return (f)
}
nrm2 = function(x) 
{
  m=0
  s=1
  f = (1/(sqrt(2*pi)*s))*exp(-0.5*((x-m)/s)^2)
  return (f)
}
fx1=nrm(x)
fx2=nrm2(x)
#nrm(1)
#dnorm(1,mean = 1 , sd =2)#to verify if our function is working fine
#fx=nrm(x)
plot(x,fx1,type="l",lwd=3,col="cyan" ,xlab ="value of X",ylab="PDF",main = "PDF OF NORMAL DISTRIBUTION")
#abline(v=1,lwd =3,col="red")
s=c("mu=1","sd=2")
legend(locator(1),legend=s,horiz = FALSE)
lines(x,fx2,lwd=3,col="yellow")
#try xlim ylim commands
s1=c("mu=0","sd=1")
legend1(locator(1),legend=s1,horiz = FALSE)


##histogram

#classes
lowerBound =seq(from=100  ,to = 700 , by =100)
upperBound =seq(from =200 , to = 800 , by = 100)
f=c(2,10,15,20,13,8,3)
df =data.frame(lowerBound , upperBound , f)
#as histogram takes ungrouped data we have to convert our grouped data to ungrouped data
br=c(100,upperBound) #breaks
m=(lowerBound+upperBound)/2
obs=rep(m,f)
hist(obs,breaks=br,col="cyan",main="Histogram",xlab="Classes",ylab="Frequency")

##if else
##frequency distribution using if else
lb=seq(100,300,100)
ub=seq(200,400,100)
obs=c(127,254,289,256,233,118,119,325,399,336)
f1=0
f2=0
f3=0

for(i in 1:length(obs)){
  if((obs[i]>=100 )&& ( obs[i]<200)){
    f1=f1+1
  }
  else if((obs[i]>=200 )&& ( obs[i]<300)){
    f2=f2+1
  }
  else if((obs[i]>=300 )&& ( obs[i]<400)){
    f3=f3+1
  }
}
fr=c(f1,f2,f3)
fr
data.frame(lb,ub,fr)

##Convergence of Sequence
#suppose Sn n>=1 is a sequence of real numbers then it is said to be convergent
#and converges to number l if for every epsilon >0 there exists a natural number 
#n0 depending on epsilon such that 
#abs(Sn-l)<epsilon for all n>n0

#Sn=1/n , l =0

#use while loop

