rm(list=ls(all=T))
setwd('U:\\uf\\courses\\bayesian course\\2018\\biomass recovery')
param=read.csv('estimated parameters.csv',as.is=T)

#0) What is the mean and 95% CI of the parameters?
apply(param,2,mean)
apply(param,2,quantile,c(0.025,0.975))

#1) What is the posterior distribution for biomass for fertil=3 and time1=10 years?
fertil=3
time1=10
res=exp(param$b0+param$b1*fertil)*time1/(param$b+time1)
plot(density(res),type='l')

#2) What is the mean predicted biomass for fertil=3 and time1=10 years?
mean(res)

#3) Besides a point estimate, give me some measure of uncertainty for the mean predicted biomass for fertil=3 and time1=10 years?
quantile(res,c(0.025,0.975))

#4) Say that there is a funding mechanism that provides funds for forest areas with biomass > 6. 
#What is the probability that the average biomass will be greater than 6?

mean(res>6)

#4) What does the relationship between time and biomass look like for fertil=3 without uncertainty?
time1=0:100
ntime=length(time1)

res=rep(NA,ntime)
for (i in 1:ntime){
  tmp=exp(param$b0+param$b1*fertil)*time1[i]/(param$b+time1[i])
  res[i]=mean(tmp)
}
plot(time1,res,type='l')

#5) What does the relationship between time and biomass look like for fertil=3 with uncertainty?
time1=0:100
ntime=length(time1)

res=matrix(NA,ntime,3)
for (i in 1:ntime){
  tmp=exp(param$b0+param$b1*fertil)*time1[i]/(param$b+time1[i])
  res[i,]=quantile(tmp,c(0.025,0.5,0.975))
}
plot(time1,res[,2],type='l',ylim=range(res))
lines(time1,res[,1],col='grey')
lines(time1,res[,3],col='grey')

#6) Contrast that with the relationship between time and biomass for fertil=1
fertil=1
res=matrix(NA,ntime,3)
for (i in 1:ntime){
  tmp=exp(param$b0+param$b1*fertil)*time1[i]/(param$b+time1[i])
  res[i,]=c(mean(tmp),quantile(tmp,c(0.025,0.975)))
}
lines(time1,res[,1],type='l',col='red')
lines(time1,res[,2],col='pink')
lines(time1,res[,3],col='pink')

