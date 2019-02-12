########
########https://bayescourse.updog.co/8A_MLE.html
param<- read.csv("estimated parameters.csv", header=T)
param
###calculate mean and 95% CI for the params
m.b1<-mean(param$b0)
m.b2<-mean(param$b1)
m.b3<-mean(param$b)
m.theta<-mean(param$theta2)
###for gamma distribution
mean<- c(m.b1,m.b2,m.b3,m.theta)
mean
c1<-quantile(param$b0,c(0.025,0.975))
c2<-quantile(param$b1,c(0.025,0.975))
c3<-quantile(param$b,c(0.025,0.975))
c4<-quantile(param$theta,c(0.025,0.975))
CI<- c(c1,c2,c3,c4)
mean
CI

#########calcualte the expression for biomass#depends on B0, b1, and b
#########for fertility of 3, and time for 10 years
######obtain post distribution for mean biomass
# E[B]= exp (b0+b1F)*t/b+t

library(dplyr)
param2= exp(param$b0+param$b1*3)*10/(param$b+10)
#pdf

plot(density(param2),type='l')
mean(param2)
quantile(param2, c(0.025,0.975))

##what is the probability that biomass >6 for f=3, t=10?
#integral of distribution of area under the curve

mean(param2>6)#in R param2>6 generate length (param2) numbers of true and false
#if we sum (param2>6), gives us count of samples >6, as it is summing number of trues

######plot time and biomass, for fertility=3
time= seq(1,100, by=1)
param3= exp(param$b0+param$b1*3)*time/(param$b+time)
length(param3)
plot(time, param3[1:100], type='l')
#### at each time step
time= seq(0,100, by=1)
ntime= length(time)
 
#empty matrix
res=rep(NA, ntime)
res1<- matrix(NA,ntime,3)


for (i in 1:ntime) {
param3= exp(param$b0+param$b1*3)*time[i]/(param$b+time[i])
res1[i,]= quantile(param3,c(0.025,0.5,0.975))
print(res1)
  }
  
  plot(time,res1[,2], type='l', ylim= range(res1))
  lines(time, res1[,1], type = 'l', col='blue')
  lines(time, res1[,3], type = 'l', col='blue')
  