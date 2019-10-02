library(rpart)




#learning rate=0.1 and depth=5 -
set.seed(1)
x1 = sort(seq(-10,10,by=.01))
y1 =sin(x1) + 5*cos(2*x1) - 3*sin(3*x1) + (1-exp(-x1/3)) +25


df1=data.frame(x1,y1)

#Setting up model
alpha1=0.1
fit1=rpart(y1~x1,data=df1,maxdepth = 4)
yp1=predict(fit1)
df1$yr1=df1$y1 - alpha1*yp1
YP1=alpha1*yp1

#loop for 200 iterations
for(t1 in 1:200){
  fit1=rpart(yr1~x1,data=df1,maxdepth = 4)
  yp1=predict(fit1,newdata=df1)
  df1$yr1=df1$yr1 - alpha1*yp1
  YP1=cbind(YP1,alpha1*yp1)}

nd1=data.frame(x1=seq(-10,10,by=.01))


#Max depth learning rate=0.5

set.seed(1)
x = sort(seq(-10,10,by=.01))
y =sin(x) + 5*cos(2*x) - 3*sin(3*x) + (1-exp(-x/3)) +25


df=data.frame(x,y)
#model

alpha=0.5
fit=rpart(y~x,data=df)
yp=predict(fit)
df$yr=df$y - alpha*yp
YP=alpha*yp

#loop for iteration
for(t in 1:200){
  fit=rpart(yr~x,data=df)
  yp=predict(fit,newdata=df)
  df$yr=df$yr - alpha*yp
  YP=cbind(YP,alpha*yp)}


nd=data.frame(x=seq(-10,10,by=.01))


#learning rate=0.05 and depth=7
set.seed(1)
x2 = sort(seq(-10,10,by=.01))
y2 =sin(x2) + 5*cos(2*x2) - 3*sin(3*x2) + (1-exp(-x2/3)) +25


df2=data.frame(x2,y2)


alpha2=0.05
fit2=rpart(y2~x2,data=df2,maxdepth = 3)
yp2=predict(fit2)
df2$yr2=df2$y2 - alpha2*yp2
YP2=alpha2*yp2
for(t2 in 1:200){
  fit2=rpart(yr2~x2,data=df2,maxdepth = 3)
  yp2=predict(fit2,newdata=df2)
  df2$yr2=df2$yr2 - alpha2*yp2
  YP2=cbind(YP2,alpha2*yp2)}

nd2=data.frame(x2=seq(-10,10,by=.01))
rmse = yp2
#function for iteration
alphaiz=function(M){
  y=apply(YP[,1:M],1,sum)
  y1=apply(YP1[,1:M],1,sum)
  plot(df$x,df$y,type='l',ylab="",xlab="",lwd=1)
  lines(df$x,y,type="s",col="red",lwd=1)
  lines(df1$x1,y1,type="s",col="green",lwd=1)
  fit1=rpart(y1~x1,data=df1,maxdepth = 4)
  yp1=predict(fit1,newdata=nd1)
  lines(nd1$x1,yp1,type="s",col="purple",lwd=1)
  
  fit=rpart(y~x,data=df)
  yp=predict(fit,newdata=nd)
  lines(nd$x,yp,type="s",col="blue",lwd=1)
  lines(nd$x,sin(nd$x),lty=2)
  
  y2=apply(YP2[,1:M],1,sum)
  lines(df2$x2,y2,type="s",col="orange",lwd=1)
  fit2=rpart(y2~x2,data=df2)
  yp2=predict(fit2,newdata=nd2)
  lines(nd2$x2,yp2,type="s",col="brown",lwd=1)
  
 

}

alphaiz(200)
YP_ = data.frame(lapply(YP,abs))
YP_ = sum(YP_[,201])
YP_A1 = data.frame(lapply(YP1,abs))
YP_A1 = sum(YP_A1[,201])
YP_A2 = data.frame(lapply(YP2,abs))
YP_A2 = sum(YP_A2[,201])
YP_
YP_A1
YP_A2
