# Epidemiology-workbook
Class project for Topics in biological Statistics

#Q1 b)
par(mfrow=c(2,2)) # a 2 x 2 grid of plots
plot(as.numeric(anteaters$location),anteaters.man$resid[,1],xlab="location(code)",ylab="Residual",main="logx1 residuals")
plot(as.numeric(anteaters$location),anteaters.man$resid[,2],xlab="location(code)",ylab="Residual", main="logx2 residuals")
plot(as.numeric(anteaters$location),anteaters.man$resid[,3],xlab="location(code)",ylab="Residual",main="logx3 residuals")

#Q2,a)

attach(epidemiology.data)
plot(Time.index,No.infected,type="l", xlab="Time index (70 increments per week)",ylab="No' Infected",main="Plot of Number of People Infected Against Time")

#b)
s0<-6000 #starting value for susceptibiles
i0<-100  #starting value for infectious
N<- 6000 #Total population size
beta<-0.00011904761  #infection rate
gamma<-1/14  #recovery rate
delta.t<-0.1  #small time increment
N.time.steps<-2000
S<-numeric(N.time.steps+1) #Black vector to receive s values
I<-numeric(N.time.steps+1)

S[1]<-s0  #initial value for s
I[1]<-i0 #intitial value for I
for (i in 1:N.time.steps){
  S[i+1]<-S[i]-beta*S[i]*I[i]*delta.t
  I[i+1]<-I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
}
time.vector<-seq(0,N.time.steps*delta.t,by=delta.t) 
plot(time.vector,I,type="l",xlab="Time",ylab="No'Of Infectious",main = "Number Of Infectious Against Time pridicted by proportion=1")

#c)
errorSS<-
  function(epidemiology.data,N=1000000,R0,D=14,theta=0.3,N.time.steps=2100,delta.t=0.1){
  gamma<-1/D
  beta<-R0*gamma/N
  S0<-theta*N
  I0<-epidemiology.data$No.infected[1] 
  sample.index<-epidemiology.data$Time.index 
  S<-numeric(N.time.steps+1)
  I<-numeric(N.time.steps+1) 
  S[1]<-S0 # Initial conditions 
  I[1]<-I0 # Initial conditions 
  for (i in 1:N.time.steps){
  S[i+1]<-S[i]-beta*S[i]*I[i]*delta.t
  I[i+1]<-I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t 
  }
errorSS<-sum((epidemiology.data$No.infected- 
 I[sample.index])^2)
return(errorSS) 
  }

R0.vector<-seq(from=6,to=14,by=0.2)
errorSS.output<-numeric(length(R0.vector))

for (i in 1:length(R0.vector)){
  R0<-R0.vector[i]
  errorSS.output[i]<-errorSS(epidemiology.data,R0=R0)
}

plot(R0.vector,errorSS.output,type="l",xlab="R0",ylab="Error sum of squares",main = "Error Sum Of Squares Against R0")
 
#d)
errorSS<-
  function(epidemiology.data,N=1000000,R0,D,theta=0.3,N.time.steps=2100,delta.t=0.1){
    gamma<-1/D
    beta<-R0*gamma/N
    S0<-theta*N
    I0<-epidemiology.data$No.infected[1] 
    sample.index<-epidemiology.data$Time.index 
    S<-numeric(N.time.steps+1)
    I<-numeric(N.time.steps+1) 
    S[1]<-S0 # Initial conditions 
    I[1]<-I0 # Initial conditions 
    for (i in 1:N.time.steps){
      S[i+1]<-S[i]-beta*S[i]*I[i]*delta.t
      I[i+1]<-I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t 
    }
    errorSS<-sum((epidemiology.data$No.infected- 
                    I[sample.index])^2)
    return(errorSS) 
  }

D.vector<-seq(from=10,to=18,by=0.2)
R0.vector<-seq(from=6,to=14,by=0.2)

errorSS.output<-matrix(0,length(R0.vector),length(D.vector))

for (i in 1:length(R0.vector)){
  for(j in 1:length(D.vector)){
  R0<-R0.vector[i]
  D<-D.vector[j]
  errorSS.output[i,j]<-log(errorSS(epidemiology.data,R0=R0,D=D))
}}

#e)
persp(R0.vector,D.vector,errorSS.output,theta = 100,phi = -100)

#f)
attach(epidemiology.data)
s0<-300000 #starting value for susceptibiles
i0<-61  #starting value for infectious
N<- 1000000 #Total population size
beta<-6.857143*10**(-7)  #infection rate
gamma<-1/14  #recovery rate
delta.t<-0.1  #small time increment
N.time.steps<-2101
S<-numeric(N.time.steps+1) #Black vector to receive s values
I<-numeric(N.time.steps+1)

S[1]<-s0  #initial value for s
I[1]<-i0 #intitial value for I
for (i in 1:N.time.steps){
  S[i+1]<-S[i]-beta*S[i]*I[i]*delta.t
  I[i+1]<-I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
}
time.vector<-seq(0,N.time.steps*delta.t,by=delta.t) 
plot(time.vector,I,type="l", xlab="Time index (70 increments per week)",ylab="No' Infected",main="Plot of Number of People Infected Against Time")
points(Week*7,No.infected,pch=20,col="red")



theta<-0.3
N<-1000000
S0<-theta*N
I0<-61
beta<-6.8571*10**(-7)
gamma<-1/14
delta.t<-0.1
N.time.steps<-2101
S<-numeric(N.time.steps+1)
I<-numeric(N.time.steps+1)
S[1]<-S0
I[1]<-I0
for (i in 1:N.time.steps){
  S[i+1]<-S[i]-beta*S[i]*I[i]*delta.t
  I[i+1]<-I[i]+beta*S[i]*I[i]*delta.t-gamma*I[i]*delta.t
}
time.vector<-seq(0,N.time.steps*delta.t,by=delta.t)
plot(time.vector,I,type="l",xlab="Time",ylab="Number of People Infected (I)", main="Theta=0.3")
points(Week*7,No.infected,pch=21,col="blue")



#Q3) a)
iris.sl.aov<-aov(Sepal.Length~Species, data = iris)
summary(iris.sl.aov)
TukeyHSD(iris.sl.aov)
iris.sw.aov<-aov(Sepal.Width~Species, data = iris)
summary(iris.sw.aov)
TukeyHSD(iris.sw.aov)
iris.pl.aov<-aov(Petal.Length~Species, data = iris)
summary(iris.pl.aov)
TukeyHSD(iris.pl.aov)
iris.pw.aov<-aov(Petal.Width~Species, data = iris)
summary(iris.pw.aov)
TukeyHSD(iris.pw.aov)
 
#c)
iris.man<-manova(cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)~Species, data= iris)

#Q4) a)
aa.man<-manova(cbind(Alanine,Aspartic,Tyrosine)~Gender, data= centepieds)

#b)
summary.aov(aa.man)

#Q5) a)
Plasma.Ca<- c(16.5,18.4,12.7,14.5,11.0,10.8,39.1,26.2,21.3, 32.0,23.8,28.8)
Water.loss<- c(76,71,64,80,72,77,71,70,63,65,69,67)
Sex<- as.factor(rep(rep(c("Female","Male"),rep(3,2)), 2))
Hormone<-as.factor(rep(c("No hormone","Hormone"),rep(6,2)))

#b)
matrix<-cbind(Plasma.Ca,Water.loss)
hormone.man<-manova(matrix~Sex*Hormone)
summary(hormone.man)

summary.aov(hormone.man)

#Q6) b)
#Wilk's lambda
man<-manova(cbind(M1,M2,M3)~Treatment, data= fields)
summary(man, test="Wilks")
#Pillai trace
summary(man, test = "Pillai")
#Hotelling-Lawley statistic
summary(man, test="H")
#Roy's statistics
summary(man, test="R")


#c)
Cont.T1.man<- manova(cbind(M1,M2,M3)~Treatment,data=fields,subset=Treatment %in% c("Control","T1"))
summary(Cont.T1.man)
Cont.T2.man<- manova(cbind(M1,M2,M3)~Treatment,data=fields,subset=Treatment %in% c("Control","T2"))
summary(Cont.T2.man)
Cont.T3.man<- manova(cbind(M1,M2,M3)~Treatment,data=fields,subset=Treatment %in% c("Control","T3"))
summary(Cont.T3.man)

T1.T2.man<- manova(cbind(M1,M2,M3)~Treatment,data=fields,subset=Treatment %in% c("T1","T2"))
summary(T1.T2.man)
T1.T3.man<- manova(cbind(M1,M2,M3)~Treatment,data=fields,subset=Treatment %in% c("T1","T3"))
summary(T1.T3.man)
T2.T3.man<- manova(cbind(M1,M2,M3)~Treatment,data=fields,subset=Treatment %in% c("T2","T3"))
summary(T2.T3.man)

#d)
summary.aov(man)

#Q7)
means <- tapply(1:nrow(skulls), skulls$epoch, function(i)
  apply(skulls[i,colnames(skulls)[-1]], 2, mean))
means <- matrix(unlist(means), nrow = length(means), byrow = TRUE)
colnames(means) <- colnames(skulls)[-1]
rownames(means) <- levels(skulls$epoch)
pairs(means,
      panel = function(x, y) {
        text(x, y, levels(skulls$epoch))
      })


#Second Part

skull.ageone.man<- manova(cbind(mb,bh,bl,nh)~epoch,data=skulls,subset=epoch %in% c("c4000BC","c3300BC"))
summary.aov(skull.ageone.man)
skull.agetwo.man<- manova(cbind(mb,bh,bl,nh)~epoch,data=skulls,subset=epoch %in% c("c4000BC","c1850BC"))
summary.aov(skull.agetwo.man)
skull.agethree.man<- manova(cbind(mb,bh,bl,nh)~epoch,data=skulls,subset=epoch %in% c("c4000BC","c200BC"))
summary.aov(skull.agethree.man)
skull.agefour.man<- manova(cbind(mb,bh,bl,nh)~epoch,data=skulls,subset=epoch %in% c("c4000BC","cAD150"))
summary.aov(skull.agefour.man)

#Q8)a)
test1<-manova(cbind(X1,X2,X3)~as.factor(group), data=Q8)
summary(test1)




N<-6
attach(Q8)
Y<-as.matrix(Q8[,1:3])
sample.mean1<-apply(Y[1:N,],2,mean)
sample.mean2<-apply(Y[(N+1):(2*N),],2,mean)
sample.mean3<-apply(Y[(2*N+1):(3*N),],2,mean)
sample.mean4<-apply(Y[(3*N+1):(4*N),],2,mean)
overall.mean<-apply(Y,2,mean)
Total.SSP<-matrix(0,3,3)
for (i in 1:24){
  Total.SSP<-Total.SSP+(Y[i,]-overall.mean)%*%t(Y[i,]-overall.mean)	
}
Between.SSP<-matrix(0,3,3)
for (i in 1:4){
  sample.mean<-apply(Y[((i-1)*N+1):(i*N),],2,mean)
  Between.SSP<-Between.SSP+N*(sample.mean-overall.mean)%*%t(sample.mean-overall.mean)
}
Within.SSP<-Total.SSP-Between.SSP

group2<-c(1,2,3,4)
means<-c(5.333333,6.005556,6.161111,4.861111)


test1<-manova(means~group2, data=Q8)
summary(test1)
summary.aov(test1)

#attempt 2 Q8

View(Q8)
attach(Q8)
test1<-manova(cbind(X1, X2, X3)~as.factor(group), data=Q8)
summary(test1)
Q8
Y<-as.matrix(Q8[,1:3])
N<-6
sample.mean1<-apply(Y[1:N,],2,mean)
sample.mean2<-apply(Y[(N+1):(2*N),],2,mean)
sample.mean3<-apply(Y[(2*N+1):(3*N),],2,mean)
sample.mean4<-apply(Y[(3*N+1):(4*N),],2,mean)
overall.mean<-apply(Y,2,mean)
Total.SSP<-matrix(0,3,3)
for (i in 1:24){
  Total.SSP<-Total.SSP+(Y[i,]-overall.mean)%*%t(Y[i,]-overall.mean)
}
Between.SSP<-matrix(0,3,3)
for (i in 1:4){
  sample.mean<-apply(Y[((i-1)*N+1):(i*N),],2,mean)
  Between.SSP<-Between.SSP+N*(sample.mean-overall.mean)%*%t(sample.mean-overall.mean)
}
Within.SSP<-Total.SSP-Between.SSP
sample.mean1
Y[1,]
A<-(Y[1,]-overall.mean)
A
t(A)
A%*%t(A)
Y
Total.SSP
Between.SSP
Within.SSP
Lambda<-det(Within.SSP)/det(Within.SSP+Between.SSP)
Lambda
Q8.man<-manova(cbind(X1, X2, X3) ~ as.factor(group), data = Q8)
Q8.man
summary(Q8.man,test="W")
sample.mean1
sample.mean2
sample.mean3
sample.mean4
psi1<-sample.mean1-sample.mean4
psi1
psi2<-sample.mean2-sample.mean3
psi2
psi3<-sample.mean1-sample.mean2-sample.mean3+sample.mean4
psi3
denominator1<-2/3
denominator2<-2/3
denominator3<-4/3
tpsi1<-t(psi1)
tpsi2<-t(psi2)
tpsi3<-t(psi3)
Bpsi1<-(psi1%*%tpsi1)/(2/3)
Bpsi1
Bpsi2<-(psi2%*%tpsi2)/(2/3)
Bpsi2
Bpsi3<-(psi3%*%tpsi3)/(4/3)
Bpsi3
lambdapsi1<-(det(Within.SSP)/det(Within.SSP+Bpsi1))
lambdapsi1
lambdapsi2<-(det(Within.SSP)/det(Within.SSP+Bpsi2))
lambdapsi2
lambdapsi3<-(det(Within.SSP)/det(Within.SSP+Bpsi3))
lambdapsi3
F1<-((1-lambdapsi1)/lambdapsi1)*((24-4-3+1)/3)
F2<-((1-lambdapsi2)/lambdapsi2)*((24-4-3+1)/3)
F3<-((1-lambdapsi3)/lambdapsi3)*((24-4-3+1)/3)
F1
F2
F3
pf(F1, 3, 18, lower.tail = FALSE, log.p=FALSE)
pf(F2, 3, 18, lower.tail = FALSE, log.p=FALSE)
pf(F3, 3, 18, lower.tail = FALSE, log.p=FALSE)


