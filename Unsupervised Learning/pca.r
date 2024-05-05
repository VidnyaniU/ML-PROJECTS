n=10  #no. of individuals 
p=3   #no. of variables
x=runif(n*p , 10,20)  #random numbers from 10 to 20
dm = matrix(x,n,p,byrow=TRUE)  #converting x to data matrix
par(mfrow=c(p,p)) #to create a 3 by 3 space
#scatter plot for all the variables
for (i in 1:p) {
  for (j in 1:p) {
    plot(dm[,i],dm[,j])
  }
}
#introducing a small change in 3rd col to make it linearly related to 1st col
dm[,3]=1.32*dm[,1]+0.56 #now there is issue of multi-collinearity
#function to calculate karl pearson correlation coefficient
rho=function(x,y){
  n =length(x)
  xbar = sum(x)/n
  ybar = sum (y)/n
  cxy= sum(x*y)/n - xbar*ybar
  sx = sqrt(sum(x^2)/n - xbar^2)
  sy = sqrt(sum(y^2)/n - ybar^2)
  corrxy = cxy/(sx*sy)
  return (corrxy)
}

#correlation matrix
cmat = matrix(nrow= p , ncol = p)
for (i in 1:p) {
  for (j in 1:p) {
    cmat[i,j] = rho(dm[,i],dm[,j])
  }
}
cmat
#we do PCA on correlation matrix rather than Sample dispersion matrix in sample
e = eigen(cmat)
#to get eigen values
ev =e$values
#scree plot
plot(1:p , ev,type = "l",col = "red",xlab = "PC number",ylab = "Eigen values")
points(1:p , ev,col = "green",pch = 19)

#contribution of variance
t = sum(ev)
cs = cumsum(ev)
con  =c()
for(i in 1:p){
  con[i]=(cs[i]/t )*100 
}
contribution = data.frame("PC Numbers"=1:p ,"Percentage of Contribution" = con )

#eigen vectors
evec = e$vectors
is.matrix(evec)#checking if it is a matrix

#orthogonality check
o = evec%*% (t(evec))

#unit vector check
##using transpose
l = c()
for (i in 1:p) {
  l[i]=(t(evec[,i])) %*% evec[,i]
}
round(l)

#spectral decomposition verification
d= diag(ev)
evec %*% d %*% t(evec)  #same as cmat
cmat

#True dimension reduction
evec[,1]#1st eigen vector
evec[,2]
##so here 1st and 2nd variable are important
#PC scores 
#new data
dm1=dm %*% evec

par(mfrow=c(p,p)) #to create a 3 by 3 space
#scatter plot for all the variables
for (i in 1:p) {
  for (j in 1:p) {
    plot(dm1[,i],dm1[,j])
  }
}
cmat_dm1 = matrix(nrow= p , ncol = p)
for (i in 1:p) {
  for (j in 1:p) {
    cmat_dm1[i,j] = rho(dm1[,i],dm1[,j])
  }
}
cmat_dm1

#we need to have standardized variables to remove collinearity and apply pca
#rectify this code
cdm = matrix(n,p)
for(i in 1:p){
  cdm[,i]=(dm[,i] - mean(dm[,i]))/sd(dm[,i])
}
cdm



##using direct commands
e1 = prcomp(dm , scale = TRUE)  #scale = TRUE considers correlation
ev1 = (e1$sdev)^2
evec1=e1$rotation
ev
evec


