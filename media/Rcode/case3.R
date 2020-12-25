## 已知某公司从2000年1月至2005年5月的逐月销售量，利用
## 所学的统计知识对所建立的模型进行诊断。

sales<-c(154,96,73,49,36,59,95,169,210,278,298,245,
         200,118,90,79,78,91,167,169,289,347,375,
         203,223,104,107,85,75,99,135,211,335,460,
         488,326,518,404,300,210,196,186,247,343,
         464,680,711,610,613,392,273,322,189,257,
         324,404,677,858,895,664,628,308,324,248,272)
X<-1:length(sales)
par(mfrow=c(1,1))
plot(X,sales,type="b",main="Original values",ylab="sales")


model1 = lm(sales~X+I(sin(2*pi/12*X))+I(cos(2*pi/12*X)))

# model1 = lm(sales~X)



lines(model1$fitted.values,lty=2,col='red')#添加回归直线

print(summary(model1))

## 残差图如下，发现异方差情形
plot(predict(model1),rstandard(model1),main="Standardized Residuals",
     xlab="Fitted values",ylab="residual values")

## 进行Box-Cox变换前，通过寻找似然函数最大值求出最优的lambda
library(MASS)
bc<-boxcox(model1,lambda=seq(-0.13,1.2,by=0.01))

lambda<-bc$x[which.max(bc$y)]
cat('The optimal lambda is',lambda)

newsales = (sales^lambda-1)/lambda



model2<-update(model1,(.^lambda-1)/lambda~.)
print(summary(model2))

par(mfrow=c(2,2))

## 比较拟合直线
plot(X,sales,type = "b",main="before boxcox")
lines(model1$fitted.values,lty=2,col='red')#添加回归直线
plot(X,newsales,type = "b",main="after boxcox")
lines(model2$fitted.values,lty=2,col='red')#添加回归直线

## 比较变换之前和变换之后的残差图
plot(predict(model1),rstandard(model1),main="before boxcox",
     xlab="Fitted values",ylab="residual values",ylim = c(-3,3))
plot(predict(model2),rstandard(model2),main="after boxcox",
     xlab="Fitted values",ylab="residual values",ylim = c(-3,3))


newX = data.frame(X = 1:(length(sales)+6))#


CI = predict(model2,newX,interval = "confidence")
trCI = (lambda*CI+1)^(1/lambda)
par(mfrow=c(1,1))
matplot(newX$X,trCI,type="l",lty = c(1,5,5),
        col=c("blue","red","red"),lwd=2,xlab = "t",ylab="sales")
points(X,sales)


