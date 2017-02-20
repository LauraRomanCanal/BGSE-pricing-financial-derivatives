rm(list=ls())
library(ggplot2)

set.seed(444)
N <- 1000
T <- 1
x <- 0.7
dt <- 1 / N

Vasicek_function <- function(m, sigma, a){
  dX <- numeric ( N +1)
  dX[1] <- x
  Z <- rnorm ( N, 0, 1 )
  for ( i in 1: N )
    dX [i +1] <- dX [ i ] + a*(m -  dX [ i ]) * dt + sigma * sqrt ( dt ) * Z [ i ]
  dX <- ts (dX , start =0 , deltat =1 / N )
  return(dX)
}


V<-matrix(0,7,N+1)
m<-c(2,1,5,2,2,2,2)
sigma<-c(2,2,2,5,1,2,2)
a<-c(2,2,2,2,2,5,1)

for (i in 1:nrow(V)){
  V[i,]<-Vasicek_function(m[i],sigma[i],a[i])
}
V
color<-c("red","blue","yellow","black","green","purple","pink")

plot(V[1,], main= "Trajectories of Vasicek model",
     col="red", ylim=c(-3,6.5), xlab="t", ylab="X",type = "l")
grid(5,5,col="black")

for (i in 2:nrow(V)){
lines(V[i,], col= color[i])
}

legend("topleft",
       c("m=2, sigma=2, a=2","m=1, sigma=2, a=2","m=5, sigma=2, a=2",
         "m=2, sigma=5, a=2","m=2, sigma=1, a=2","m=2, sigma=2, a=5","m=2, sigma=2, a=1"),
       col=c("red","blue","yellow","black",
               "green","purple","pink"),
       lty=c(1,1),
       cex = 0.58)


















