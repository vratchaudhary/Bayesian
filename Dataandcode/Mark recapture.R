#####
#######
#generate some fake data
rm(list=ls(all=T))
set.seed(1)
#true parameter values
delta.true=delta=0.3
N=200 #true population size
#number of surveys 
T1=4
#observation status of each individual
D1=matrix(0,N,T1)
for (i in 1:T1){
  D1[,i]=rbinom(N,size=1,p=delta)
}

#in the observed data, we do not have individuals that were never observed
cond=apply(D1,1,sum)!=0
data1=D1[cond,]
nrow(data1) #number of individuals we get to observe at least once
###never observed cases 
cond=apply(D1,1,sum)==0
data1=D1[cond,]
nrow(data1) 
#########so you are going to remove these cases from the data, and the actual 
##data will never have individuals that were never seen 
#####now individuals can either exist or not exists, so its 0 and 1
##so zi~bernouli (pi) will be our distribution and pi is the 
##pi is the estimate of fraction of individuals that exists. 
###now what actually is pi, is used to correct

##inits <- list 

