## Case study: It is found that the systolic 
## pressure is linked to the weight and the age.

blood=data.frame(
  weight=c(76.0,91.5,85.5,82.5,79.0,80.5,74.5,
           79.0,85.0,76.5,82.0,95.0,92.5),
  age=c(50,20,20,30,30,50,60,50,40,55,40,40,20),
  pressure=c(120,141,124,126,117,125,123,125,
             132,123,132,155,147)
)

lm.blood=lm(pressure~weight+age,data=blood)

result = summary(lm.blood)

print(result)

# Estimated Covariance Matrix
print(result$cov.unscaled*result$sigma^2) 


## prediction 1: fixed age = 31

newdata = data.frame(
  age = rep(31,100),
  weight = seq(70,100,length.out = 100)
)
CI = predict(lm.blood,newdata,interval = "confidence")
Pred = predict(lm.blood,newdata,interval = "prediction")
matplot(newdata$weight,cbind(CI,Pred[,-1]),type="l",
        lty = c(1,5,5,2,2),
        col=c("blue","red","red","brown","brown"),lwd=2,
        xlab="Weight",ylab="Pressure",main = "Age = 31")
#legend(70,160,c("Fitted","Confidence","Prediction"),
#       lty = c(1,5,2),col=c("blue","red","brown"))


## prediction 2: fixed weight = 85

newdata = data.frame(
  weight = rep(85,41),
  age = seq(20,60)
)
CI = predict(lm.blood,newdata,interval = "confidence")
Pred = predict(lm.blood,newdata,interval = "prediction")
par(mar=c(4,4,2,1))
matplot(newdata$age,cbind(CI,Pred[,-1]),type="l",
        lty = c(1,5,5,2,2),
        col=c("blue","red","red","brown","brown"),lwd=2,
        xlab="Age",ylab="Pressure",main = "Weight = 85")
#legend(20,150,c("Fitted","Confidence","Prediction"),
#       lty = c(1,5,2),col=c("blue","red","brown"))