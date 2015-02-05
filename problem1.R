library(MASS)
library(plot3D)
set.seed(2)
Sigma <- matrix(c(10,1,1,2),2,2)
Sigma <- matrix(c(1,0.5,0.5,1),2,2)

var1 = mvrnorm(n=20,c(2,2),Sigma)
var2 = mvrnorm(n=20,c(4,4),Sigma)
var3 = mvrnorm(n=20,c(6,6),Sigma)
alldata = rbind(var1,var2,var3)

#######Problem 1a #############
plot(var1,col='red',pch=22,bg='red',xlim=range(0,max(alldata[,1])),ylim =range (0,max(alldata[,2])))
points(var2,col='green',pch=24,bg='green')	
points(var3,col='blue',pch=19,bg='blue')

#######Problem 1b #############
hist3D(z=alldata,ltheta = -135)
