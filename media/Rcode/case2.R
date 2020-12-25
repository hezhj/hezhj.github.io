## Case study 2: Anscombe在1973年构造了4组数据

coef.list = data.frame()
for(i in 1:4)
{
  ff = as.formula(paste0("y",i,"~","x",i))
  lmi = lm(ff,data = anscombe)
  slmi = summary(lmi)
  pvalue = 1-pf(slmi$fstatistic[1],slmi$fstatistic[2],slmi$fstatistic[3])
  df = as.data.frame(cbind(slmi$coef,p_value = c(pvalue,NA)))
  row.names(df)[1] = paste0(row.names(df)[1],i)
  coef.list = rbind(coef.list,df)
}

print(coef.list)

print(summary(lmi)) #最后一组回归结果

## 四组回归直线
par(mfrow = c(2,2),mar=c(4,4,1,1)+.1,oma=c(0,0,2,0))
for(i in 1:4)
{
  ff = as.formula(paste0("y",i,"~","x",i))
  lmi = lm(ff,data = anscombe)
  plot(ff,data = anscombe,col="red",pch = 21,bg="orange",
       cex = 1.2,xlim=c(3,19),ylim=c(3,13))
  abline(coef(lmi),col="blue")
}


## 四组残差分析

par(mfrow = c(2,2),mar=c(4,4,1,1)+.1,oma=c(0,0,2,0))
for(i in 1:4)
{
  ff = as.formula(paste0("y",i,"~","x",i))
  lmi = lm(ff,data = anscombe)
  plot(lmi$fitted.values,rstandard(lmi),pch = 21,bg="orange",
       cex = 1.2,xlim=c(3,15),ylim=c(-4,4),
       xlab = "fitted values",
       ylab = "standardized residuals")
  abline(h=c(-3,3),lty=2)
}
