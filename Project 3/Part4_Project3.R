
rm(list = ls())

library("ggplot2")


N<-1000
T<-1
delta<-T/N
t<-seq(0,T,delta)

#in order to be able to compare the trajectories of different option prices according 
# the value of the parameters, we generate first Z~N(0,1) so that 
# the only difference between the geometric BM to be their parameters
z<-rnorm(N,0,1)

#take the BM at t=0 to be 0 by taking the first element to be zero 
Z<-c(0,z)



# Create a function that simulates the geometric BM
geometricBM<- function(r,sigma){
  B<-rep(0,N+1)
  for (i in 2:N+1){
    B[i]<-B[i-1] + sqrt(delta)*Z[i]
  }
  t<-seq(0,T,delta)
  S<-r * t + sigma * B 
  
  return(exp(S))
}


#Simulate the Y_t variable that is given by the integral from 0 to t of S_t
#through Riemann sum

#difference between t_0,t_1...t_N is const= delta
#generate a vecotor of the differences between the t

computeY<-function(S){
  diff_t<-rep(delta,N)
  Y<-rep(0,N+1)
  for (i in 1:N+1){
    for (j in 1:i){
      Y[i]<-sum(Y[i], (S[j-1]*diff_t[j-1]))
    }
  }
  return(Y)
}


#Compute the price
Vt<-function(S,Y,r){
  vector_T<-rep(T,N+1)
  vector_r<-rep(r,N+1)
  Vt<-(exp(-vector_r*(vector_T-t))/vector_T)*(Y+(S/vector_r)*(exp(vector_r*(vector_T-t)-1)))
  return(Vt)
}



r<-     c( 0.5, 1, 1.5, 1, 1)
sigma<- c( 2, 2, 2, 1.5, 2.5)

S1<-geometricBM(r=r[1],sigma=sigma[1])
Y1<-computeY(S=S1)
Vt1<-Vt(S=S1,Y=Y1,r=r[1])


S2<-geometricBM(r=r[2],sigma=sigma[2])
Y2<-computeY(S=S2)
Vt2<-Vt(S=S2,Y=Y2,r=r[2])

S3<-geometricBM(r=r[3],sigma=sigma[3])
Y3<-computeY(S=S3)
Vt3<-Vt(S=S3,Y=Y3,r=r[3])

S4<-geometricBM(r=r[4],sigma=sigma[4])
Y4<-computeY(S=S4)
Vt4<-Vt(S=S4,Y=Y4,r=r[4])

S5<-geometricBM(r=r[5],sigma=sigma[5])
Y5<-computeY(S=S5)
Vt5<-Vt(S=S5,Y=Y5,r=r[5])


ggplot() +
  geom_line(aes(x = t, y = Vt1, colour = "r=0.9, sigma=2"), size = 0.2) +
  geom_line(aes(x = t, y = Vt2, colour = "r=1, sigma=2"), size = 0.2) +
  geom_line(aes(x = t, y = Vt3, colour = "r=1.2, sigma=2"), size = 0.2) +
  geom_line(aes(x = t, y = Vt4, colour = "r=1, sigma=1.9"), size = 0.2) +
  geom_line(aes(x = t, y = Vt5, colour = "r=1, sigma=2.2"), size = 0.2) +
  labs(title = "Trajectories of the Option Price", x = "t", y = "Vt")


########################################################
# Simulate b
########################################################

vector_r<-rep(1,N+1)
vector_T<-rep(1,N+1)
bt1<- (1/vector_r*vector_T)-(exp(-vector_r*(vector_T-t))/vector_r*vector_T)

vector_r2<-rep(1.5,N+1)
vector_T<-rep(1,N+1)
bt2<-(1/vector_r2*vector_T)-(exp(-vector_r2*(vector_T-t))/vector_r2*vector_T)

vector_r3<-rep(0.5,N+1)
vector_T<-rep(1,N+1)
bt3<-(1/vector_r3*vector_T)-(exp(-vector_r3*(vector_T-t))/vector_r3*vector_T)


ggplot() +
 geom_line(aes(x = t, y = bt1, colour = "r=1"), size = 0.2) +
  geom_line(aes(x = t, y = bt2, colour = "r=1.5"), size = 0.2) +
  geom_line(aes(x = t, y = bt3, colour = "r=0.5"), size = 0.2) +
  labs(title = "Trajectory of the strategy", x = "t", y = "b_t")

