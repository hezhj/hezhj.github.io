if(!require(dslabs))
  install.packages("dslabs")
attach(heights)


# NB: the data "heights" are measured in inches

# frequency
r=hist(height)
text(r$mids,r$counts,r$counts,adj=c(.5,-.5),
     cex=1.2)

# density
hist(height,freq=FALSE)


# kenerel density
plot(density(height,from = 50,to=85),col="red",
     lwd=2,main="Kernel density")

# Histogram vs. Kernel density
hist(height,ylim=c(0,.115),freq=FALSE,
       main="Histogram vs. Kernel density")
lines(density(height,from = 50,to=85),col="red",lwd=2)


# female vs. male
female_height = height[sex=="Female"]
male_height = height[sex=="Male"]

plot(density(male_height,from = 50,to=85),
     col="red",lwd=2,ylim=c(0,.14),main="Male vs. Female")

lines(density(female_height,from = 50,to=85),
      col="blue",lwd=2)
legend(74,0.12,legend = c("Male","Female"),
       lty = c(1,1),col=c("red","blue"),lwd=c(2,2))

detach(heights)

## ggplot
library(ggplot2)
ggplot(heights,aes(height,fill =sex,colour = sex))+
  geom_density(alpha = 0.1)+
  theme_classic()

## Violin plot + box plot
ggplot(heights,aes(x=sex,y=height,fill=sex))+
  geom_violin()+
  geom_boxplot(width=0.1, fill="white")+
  theme_classic()
