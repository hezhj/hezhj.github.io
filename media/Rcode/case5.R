## case study 5: 某种水泥在凝固时放出的热量 
## 与水泥中四种化学成分有关

cement=data.frame(
  x1=c(7,1,11,11,7,11,3,1,2,21,1,11,10),
  x2=c(26,29,56,31,52,55,71,31,54,47,40,66,68),
  x3=c(6,15,8,8,6,9,17,22,18,4,23,9,8),
  x4=c(60,52,20,47,33,22,6,44,22,26,34,12,12),
  y=c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,
      93.1,115.9,83.8,113.3,109.4))


lm_all=lm(y~x1+x2+x3+x4,data=cement)
print(summary(lm_all))

lm0 = lm(y~1,data=cement) #只有截距项作为初始模型
lm.step = step(lm0,direction = "both",scope = ~x1+x2+x3+x4)

print(summary(lm.step))

## 剔除第四个变量后的结果如下，所有的检验都是显著的，残差诊断也合理。
lm_final = update(lm.step,.~.-x4)
print(summary(lm_final))
par(mfrow=c(1,1))
plot(predict(lm_final),rstandard(lm_final),main="new model",
     xlab="Fitted values",ylab="residual values",ylim = c(-3,3))