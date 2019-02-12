n=400
z=rbinom(n,size=1,prob=0.2)
a.prior= 1 ##just real number parameters
b.prior= 1
pi.old= 0.5 
###t(pi.new)*q(pi.old|pi.new)/t(pi.old)*q(pi.new|pi.old)=
# exp(log|t(pi.new)*q(pi.old|pi.new)/t(pi.old)*q(pi.new|pi.old))=
#exp(log|t(pi.new)+log|q(pi.old|pi.new))-log|t(pi.old)-log|q(pi.new|pi.old))
pi.new=runif(1)
##either accept or reject pi.new
#now find old target pi from log|t(pi.old)+log|q(pi.new|pi.old))
target.pi.old=sum(dbinom(z,size=1,prob=pi.old,log=T)+
                    dbeta(pi.old,a.prior,b.prior,log=T))
###now writing the same expression for pi.new
target.pi.new=sum(dbinom(z,size=1,prob=pi.new,log=T)+
                    dbeta(pi.new,a.prior,b.prior,log=T))
pthresh=min(c(1, exp(target.pi.new-target.pi.old)))
rand=runif(1)
accept=rand<pthresh
accept

