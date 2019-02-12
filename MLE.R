#I poisson lamda is mean and variance, which is usually a continuous variable. For posson link function is usually loglambda.
#yi~ poisson (lambda i)
#lambdai = exp( beta 0+ beta1 xi)# this is correct as lambda cant be 0 and exponential is ensuring that
#say x1= elavation
#x2= ndvi
#y= number of trees

######creating fake data
#y= poisson (lambda)
##lambdai = exp( beta 0+ beta1 xi)# 

b0=b1=b2=1
###creating standardized data by taking normal values with mean and variance of 0 and 1
elevation = rnorm(100,0,1)##for every tree we need an eleavtion value, length of data and mean and variance
ndvi= rnorm(100,0,1)

lambda=exp(b0+b1*elevation+b2*ndvi)
y=rpois(100,lambda)
hist(y)
data=data.frame(y=y, x1=eleavation, x2=ndvi)

####MLE
# you need your likelihood when you are writing your MLE
###we said our data come from a poisson distribution
###Analytically you would create loglikelihood, in R you use a negative loglikeihood as 
#the optimizer is the minimzer in R, not a maximizer

##create a function
negloglik<- function(dat, par){
  b0= par[1]
  b1=par[2]
  b2=par[3]
  #lambda
  lambda=exp(b0+b1*dat$x1+b2*dat$x2)
  #or lambda=exp(b0+b1*dat[,2]+b2*dat[,3])
###the sum of neg log likelihood
LL<- -sum(dpois(dat$y,lambda,log=T )) # dpos= prob density, rpos= random poisson, qpos= quantile pos function
##we are looking for density
####we are summing the area under the curve, and it has to be negative as we are looking for negative loglikelihood
return(LL)
}

##initial parameters
b0=b1=b2=0
init_par=c(b0,b1,b2)

####MLE, nelder mead method is the default method
?optim
M.like<- optim(par=init_par, fn=negloglik,dat=data)