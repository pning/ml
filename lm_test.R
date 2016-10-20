# Residuals 残差 ; Coefficients 系数 ; Multiple R-squared 相关系数 ; p-value P值
y=c(67,57,58,40,90,35,68)
x=c(178,168,175,153,185,135,172)
plot(x,y)
z=lm(y~x+1)
summary(z)
plot(z)
m=lm(y~x-1)
summary(m)
plot(m)
z=data.frame(x=185)
predict(m,z)

swiss_lm=lm(Fertility~.,data=swiss)
summary(swiss_lm)
plot(swiss_lm)

sl=step(swiss_lm,direction = "backward")
sl=step(swiss_lm,direction = "both")
drop1(sl)

cement<-data.frame(
                   X1=c(7,1,11,11,7,11,3,1,2,21,1,11,10),
                   X2=c(26,29,56,31,52,55,71,31,54,47,40,66,68),
                   X3=c(6,15,8,8,6,9,17,22,18,4,23,9,8),
                   X4=c(60,52,20,47,33,22,6,44,22,26,34,12,12),
                   Y=c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
                   )
lm.sol<-lm(Y~X1+X2+X3+X4,data=cement)
summary(lm.sol)
lm.step<-step(lm.sol,direction = "backward")
summary(lm.step)
drop1(lm.step)
lm.opt<-lm(Y ~ X1+X2, data=cement)
summary(lm.opt)
shapiro.test(cement$X3)

Anscombe<-data.frame(
X=c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0),
Y1=c(8.04,6.95, 7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68), 
Y2=c(9.14,8.14, 8.74,8.77,9.26,8.10,6.13,3.10, 9.13,7.26,4.74), 
Y3=c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39, 8.15,6.44,5.73), 
X4=c(rep(8,7), 19, rep(8,3)), 
Y4=c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.50, 5.56,7.91,6.89)
)
summary(lm(Y1~X,data=Anscombe))
summary(lm(Y2~X,data=Anscombe))
summary(lm(Y3~X,data=Anscombe))
summary(lm(Y4~X,data=Anscombe))
plot(lm(Y1~X,data=Anscombe))
plot(lm(Y2~X,data=Anscombe))
plot(lm(Y3~X,data=Anscombe))
plot(lm(Y4~X,data=Anscombe))

y.res<-residuals(lm.sol)
shapiro.test(y.res)
y.res<-resid(lm.sol)
y.fit<-predict(lm.sol)
plot(y.res~y.fit)


X<-scan()
Y<-scan()
lm.sol<-lm(Y~X)
summary(lm.sol)
y.rst<-rstandard(lm.sol)
y.fit<-predict(lm.sol)
plot(y.rst~y.fit)

lm.new<-update(lm.sol, sqrt(.)~.)
coef(lm.new)
summary(lm.new)
yn.rst<-rstandard(lm.new)
yn.fit<-predict(lm.new)
plot(yn.rst~yn.fit)


collinear<-data.frame(
  Y=c(10.006, 9.737, 15.087,8.422,8.625,16.289, 5.958,9.313, 12.960,5.541,8.756,10.937),
  X1=rep(c(8, 0, 2, 0), c(3,3,3,3)),
  X2=rep(c(1, 0, 7, 0), c(3,3,3,3)),
  X3=rep(c(1, 9, 0), c(3, 3,6)),
  X4=rep(c(1, 0, 1, 10), c(1, 2, 6, 3)), 
  X5=c(0.541, 0.130, 2.116, -2.397, -0.046, 0.365,1.996, 0.228, 1.38, -0.798, 0.257, 0.440 ),                                                                                                                                                                                     
  X6=c(-0.099, 0.070, 0.115, 0.252, 0.017, 1.504,-0.865, -0.055, 0.502, -0.399, 0.101, 0.432)                                                                                                                             
)
XX<-cor(collinear[2:7])
kappa(XX,exact=TRUE)
eigen(XX)

a=c(0,1,2,3,4,5)
b=c(0.000,0.129,0.300,0.671,0.857,0.900)
plot(a~b)
norell<-data.frame(x=0:5,n=rep(70,6),successc(0,9,21,47,60,63))


