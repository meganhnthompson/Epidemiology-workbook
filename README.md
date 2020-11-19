# Epidemiology-workbook
Class project for Topics in biological Statistics

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

D.matrix<-matrix(D.vector)
R0.matrix<-matrix(R0.vector)
z<-cbind(D.matrix, R0.matrix)
persp(R0,D,z,theta = 0,phi = 15)

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




