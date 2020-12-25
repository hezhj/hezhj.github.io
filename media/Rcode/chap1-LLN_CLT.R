## illustration of Law of Large Numbers (LLN)
set.seed(777) # set the initial seed

n = 1e3 # sample size

# exponential distribution
lam = 10
x = rexp(n,lam) 
true_val = 1/lam


# weak LLN 
par(mfrow=c(2,2))
ns = c(1,5,10,100)
h = true_val/5
for(i in ns)
{
  denFun = function (x) dgamma(x,i,i*lam) # density function
  plot(denFun, 0, 0.6,
       xlab="",ylab="",main=paste0('sample size = ',i))
  abline(v=c(true_val-h,true_val+h),lty=2)
}

# strong LLN
par(mfrow=c(1,1))
x_bar = cumsum(x)/(1:n)
plot(1:n,x_bar,pch=20,type = 'b',xlab='sample size',
     ylab='sample mean')
abline(h=true_val,lty=2)


# the Cauchy Distribution (does not converge)
n = 1e4
par(mfrow=c(1,1))
x = rcauchy(n) 
x_bar = cumsum(x)/(1:n)
plot(1:n,x_bar,pch=20,type = 'b',xlab='sample size',
     ylab='sample mean')


## illustration of central limit theorem (CLT)

# exponential distribution

par(mfrow=c(2,2))
for(i in ns)
{
  mu = true_val
  sig = 1/lam/sqrt(i)
  denFun = function (x) sig*dgamma(mu+sig*x,i,i*lam) # density function after normalizing
  plot(denFun, -3, 3,
       xlab="",ylab="",main=paste0('sample size = ',i))
  curve(dnorm,-3,3,add=T,col='red',lty=2)
}



