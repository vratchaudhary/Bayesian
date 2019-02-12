#samp.pi
samp.pi= function (param.N){
  soma.21= sum(param$z1)
  a1=soma.21=z1+a.pi
  b1=N-soma.Z1-b.pi
  rbeta(1,a1,bi)
  
}#delta
samp.delta=sunction(param,T1){
  cond=parama$z1==1
  tot.detect =sum(data[cond])
  tot.nopport= sum(cond)*T1
  a1=tot.detect+a.delta
  b1=tot.noapport+tot.detect+b.delta
  for(i in 1:T1){
   delta[i]<-rbeta(1,a1,b1)
  }
  delta
}

###samp.z
samp.z1= function(param,T1){
prob1.tmp=((1-param$delta)^T1) * param$pi
prob0.tmp=(1-param$pi)
prob1=prob1.tmp/(prob1.tmp-prob0.tmp)
cond=apply(dat,1,sum)==0
z1=rep(1,N)
z1[cond]=rbinom(sum(cond), size=1,prob1)
z1
  
  
}

############
#############Creating fake data#######
rm(list=ls())
set.seed(1)
#THE POPULATION
N=400
#surveYs
T1=4
#starting deltas
delta.true=c(0.5,0.5,0.5,0.5)
###data
D1= matrix(NA,N,T1)
#lets fill the empty matrix for each survey
for(i in 1:T1){
  D1[,i]= rbinom(N,size = 1, prob=delta.true[i])
  
}

cond=apply(D1,1,sum)!=0##create a condition for the data so that not everything is o
data=D1[cond,]
nrow=(data)
#write.csv(data,"testdata_pop.csv")

#########Gibbs sampler
rm(list=ls())
set.seed(1)
data=read.csv("testdata_pop.csv")

#specify function
N=400
T1=4
#INTIAL PARAMS
pi= 0.5
delta=rep(0.5,T1)
z1=rep(1)
delta=rep(NA,T1)



