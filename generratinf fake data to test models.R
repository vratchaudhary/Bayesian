x1=2*pi*seq(from=0,to=,length.out=1000)
y=sin(x1)
plot(x1,y)
#########
b0=2
b1=3
b2=4
sigma=0.3
n=1000
x1=runif(n)
x2=runif(n)
y=rnorm(n,mean=b0+b1*x1+2*x2,sd=sqrt(sigma))
dat=data.frame(y=y,x=x1,x2=x2)

##testing the model
res=lm(y~x1+x2,data=dat)
summar(res)###check your model if the coeffcients ~ b0,b1, b2 vlues you put
??rburn
###bernoulu
