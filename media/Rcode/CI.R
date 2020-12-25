# generate data from exponential distribution
set.seed(0)

lambda <- 2
n <- 1000
X <- rexp(n,lambda)

# find out the confidence interval (CI)
alpha <- 0.05
a <- qchisq(p=alpha/2,df=2*n)
b <- qchisq(p=1-alpha/2,df=2*n)
cat('sample size n =', n, ', true lambda = ', lambda, '\n', sep="")
CI <- c(a/2/sum(X),b/2/sum(X))
cat('point estimate is ', 1/mean(X),'\n',sep="")
cat((1-alpha)*100,"% CI is [",CI[1],", ", CI[2],"]", '\n',sep="")

# generate R batches of the data

R <- 100
CIs <- matrix(0,R,3)
for(i in 1:R){
  X <- rexp(n,lambda)
  CIs[i,] = c(a/2/sum(X),1/mean(X),b/2/sum(X))
}

## plot the CIs

plot(0, xlim=c(0, R), ylim=c(min(CIs)-0.02,
      max(CIs)+0.02), type="n",xlab="Sample ID",ylab="", main = 'CIs for 100 replications')

count <- 0
for (i in 1:nrow(CIs)) {
  if (CIs[i, 1]>lambda | CIs[i, 3]<lambda){
    color = "red"
    count = count +1
    if(CIs[i, 1]>lambda)
      text(i,CIs[i, 3]+0.02,count)
    else
      text(i,CIs[i, 1]-0.02,count)
  }else{
    color = "blue"
  }
  lines(x=rep(i, 2), y=c(CIs[i, 1], CIs[i, 3]))
  points(x=i, y=CIs[i,2], pch=16, col=color)
}
abline(h=lambda,lty = 2,col="red",lwd=3)

