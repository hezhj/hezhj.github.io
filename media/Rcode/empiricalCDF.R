for(n in c(5, 10, 50, seq(200,1000,by=200))){
#standard normal data (simulated)
data  = rnorm(n)
hatF = ecdf(data) #经验分布函数命令
#阶梯形式
plot(hatF,main=paste0('n = ',n),xlim=c(-3,3))
plot(hatF,verticals = TRUE, do.points = FALSE,
     main=paste0('n = ',n),xlim=c(-3,3), ylab='ECDF')
curve(pnorm,add=TRUE,col='red',lty=2)#画真实密度图像
Sys.sleep(1)
}
