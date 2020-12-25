## 离群值

lm1 = lm(y1~x1,data = anscombe)
x = c(anscombe$x1, 18) # 人为添加异常数据
y = c(anscombe$y1,30) # 人为添加异常数据
lm.xy = lm(y~x)
par(mfrow=c(1,1))
plot(x,y,pch = 21,bg=c(rep("black",11),"red"),ylim=c(0,50))
abline(coef(lm.xy),lty=2,col="red")
abline(coef(lm1),lty=2,col="blue")

par(mfrow=c(1,2))
plot(c(fitted(lm1),fitted(lm.xy)),
     c(rstandard(lm1),rstandard(lm.xy)),
     ylim=c(-3,3),pch = c(rep(21,11),rep(22,12)),
     bg = c(rep("blue",11),rep("red",12)),
     xlab = "fitted values",
     ylab="standardized residuals")
abline(h=c(-2,2),lty=2)
plot(lm.xy,4) #画出第二组数据的cook距离
