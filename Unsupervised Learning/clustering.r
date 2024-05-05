#car data
data(mtcars)
head(mtcars)
is.matrix(mtcars) #not in a matrix form
n = nrow(mtcars) #no of objects
p = ncol(mtcars) #no of variables

#preparing data matrix
x=c()
for (i in 1:p) {
  x=c(x,mtcars[,i])
}
x
dm = matrix(x,nrow= n , ncol = p, byrow = FALSE)
dm
#normalizing data
sdm = matrix(nrow = n , ncol = p)
for (i in 1:p){
  sdm[,i]=(dm[,i] - mean(dm[,i])) /(sd(dm[,i]))
}
sdm
###################
#investigating correlation structure
pairs(dm)
round(cor(sdm),2)

##### k means clustering where k =3 #####
k = 3
km=kmeans(sdm , centers = k , nstart = 32)
km$cluster
km$size

#calculating total ss
tss = c()
#k must lie between 1 and n 
for(i in 1:(n-2)){
  temp=kmeans(sdm,centers  =i+1 , nstart = 32)
  tss[i]=temp$tot.withinss
}
tss
length(tss)
plot(1:(n-2) ,tss , type = "l",lwd =5 , col = "red",xlab = "No of cluster",ylab= "TSS", main = "Elbow plot for TSS")
points(1:(n-2),tss ,pch = "*",col = "yellow")
##adding horizontal lines
for (i in 1:(n-2)){
  abline(h=tss[i],type="dashed")
}

################################
## Hierarchical agglomerative ##
################################
#Ward Hierarchical Clustering#
dis = dist(sdm , method = "euclidean") #distance matrix
fit = hclust(dis , method = "ward.D")
fit$merge
plot(fit) #display dendogram
groups = cutree(fit , k =4 )
rect.hclust(fit , k=4 , border = "red")
