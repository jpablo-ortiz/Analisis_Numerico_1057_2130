# Remove-------------------------------------------------------------------
rm(list=ls())
gc()  # garbage collector
#--------------------------------------------------------------------------
library(rgl) 
library (ggplot2)
library(PolynomF)

# Punto 1 y 2

SIR.model <- function(t, b, g){ # function of t, b and g
  require(deSolve) # call in of the deSolve package
  N = 779853
  init <- c(S=1-(2/779853),I=2/779853,R=0)
  parameters <- c(beta=b,gamma=g) #paramters in the ode
  time <- seq(0,t,by=t/(2*length(1:t))) #time sequence for the ode solution
  eqn <- function(time,state,parameters){ #SIR odes
    with(as.list(c(state,parameters)),{ #solve the ode using the parameters
      dS <- -beta*S*I #change in proportion of susceptibles (dS/dt)
      dI <- beta*S*I-gamma*I #change in proportion of infected (dI/dt)
      dR <- gamma*I #change in proportion of the recovered (dR/dt)
      return(list(c(dS,dI,dR)))}) #out as a list containing the values
  }
  out<-ode(y=init,times=time,eqn,parms=parameters, method = "rk4") #solve the ode using ode() in deSolve package
  out.df<-as.data.frame(out) #create a data frame of the output of ode()
  
  require(ggplot2) #call in ggplot2 package
  mytheme4 <- theme_minimal()
  theme_set(mytheme4) #http://docs.ggplot2.org/current/theme_update.html
  title <- bquote("Modelo SIR: Propagación COVID-19 Santa Marta") #title for plot
  subtit<-bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]))) #use of bquote to include Greek symbols of beta and gamma into subtitle
  res<-ggplot(out.df,aes(x=time))+ #set plot of ode data frame output and x-variable as time
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+ # create the title and subtitle based on http://stackoverflow.com/q/30338719/6168956
    geom_line(aes(y=S,colour="Susceptibles"))+ #assign plot line as S from out.df
    geom_line(aes(y=I,colour="Infectados"))+ #assign plot line as I from out.df
    geom_line(aes(y=R,colour="Recuperados"))+ #assign plot line as R from out.df
    ylab(label="Proporción")+ #y-axis label
    xlab(label="Tiempo (días)")+ #x-axis label
    theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend justification - anchorpoint of legend, legend.position based on two-element numeric vector (x,y)
    theme(legend.title=element_text(size=12,face="bold"), #set font specification of title
          legend.background = element_rect(fill='#FFFFFF',size=0.5,linetype="solid"), #legend background set to white
          legend.text=element_text(size=10), #set legend text size
          legend.key=element_rect(colour="#FFFFFF", #set legend keys border to white
                                  fill='#C2C2C2', #fill set to gray
                                  size=0.25, #size of border
                                  linetype="solid"))+ #line type of border
    scale_colour_manual("Líneas", #title of legend
                        breaks=c("Susceptibles","Infectados","Recuperados"), #each level of lines, set to colour
                        values=c("blue","red","darkgreen")) #colours for each respective level
  
  print(res) #print output of plot
  ggsave(plot=res, # call plot name
         filename=paste0("SIRplot_","time",t,"beta",b,"gamma",g,".png"), #set the filename with parameters of time, beta and gamma
         width=5.75,height=4,dpi=120) #dimensions and resolution of .png file
  getwd() #display working directory for saved .png file location
  indiceControl = g/(b*c)
  c <- which(out.df[['S']]<indiceControl)-1
  cat("\n\nApartir del día ", c[1] ," (aproximadamente marzo 27 del 2021) se estima que la epidemia estará controlada.")
}
SIR.model(652,0.06,0.021)

# Parametros
Beta<-0.06
c <-1.5
gama<- 0.021
N1<-779853

# Condiciones iniciales 
I0<-2/N1
S0<-1-I0
R0<-0

paso<-1

# Numero de pasos (dias)
dias<-652

S<-function(x,y,z,t) -Beta*x*y
I<-function(x,y,z,t) Beta*x*y-gama*y
R<-function(x,y,z,t) gama*y

vS<-replicate(dias+1,0)
vI<-replicate(dias+1,0)
vR<-replicate(dias+1,0)

vS[1]<-S0
vI[1]<-I0
vR[1]<-R0

#------Punto 3
p<-2
while(p<=dias+1){
  xi<-vS[p-1]
  yi<-vI[p-1]
  zi<-vR[p-1]
  
  k1<-paso*S(xi,yi,zi,ti)
  l1<-paso*I(xi,yi,zi,ti)
  m1<-paso*R(xi,yi,zi,ti)
  
  k2<-paso*S(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  l2<-paso*I(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  m2<-paso*R(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  
  k3<-paso*S(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  l3<-paso*I(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  m3<-paso*R(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  
  k4<-paso*S(xi+k3,yi+l3,zi+m3,ti+paso)
  l4<-paso*I(xi+k3,yi+l3,zi+m3,ti+paso)
  m4<-paso*R(xi+k3,yi+l3,zi+m3,ti+paso)
  
  vS[p]<-xi+(1/6)*(k1+2*k2+2*k3+k4)
  vI[p]<-yi+(1/6)*(l1+2*l2+2*l3+l4)
  vR[p]<-zi+(1/6)*(m1+2*m2+2*m3+m4)
  
  p<-p+1
}

MaxInfectados<- max(vI)
MaxInfectados*N1
DiaMaxInfectados<-which.max(vI)
DiaMaxInfectados

#------- Punto 4

MaxRecuperados<- max(vR)
MaxRec*N1
DiaMaxRec <- which.max(vR)
DiaMaxRec

PorcentajeInfectados<- MaxInfectados
PorcentajeInfectados
PorcentajeRecuperados<- MaxRecuperados
PorcentajeRecuperados


#----- Punto 5

#  Como I(t)=C/N
#  gama/(Beta*C)>S/N
#  gama/(Beta*I(t))>S(t)

control<-integer(dias)

for (i in 1:dias){
 if (gama/(Beta*vI[i])>vS[i]){
   control[i]<-1
 } 
}

plot(control)

#-----------Punto 6


#----------Punto 7

vectorRe<- integer(60)
for(i in 1:60){
  vectorRe[i]<-Beta*c*vS[i]/(gama*N1)   #(Beta*vI[i]*vS[i])/gama
}

max(vectorRe)

plot(vectorRe, type="l")

#----------Punto 8


#----------Punto 9
# Solucion Exacta
dias <- 71
vS2<-replicate(dias+1,0)
vI2<-replicate(dias+1,0)
vR2<-replicate(dias+1,0)

vS2[1]<-1
vI2[1]<-14/N1
vR2[1]<-7/N1


p<-2
while(p<=dias+1){
  xi<-vS[p-1]
  yi<-vI[p-1]
  zi<-vR[p-1]
  
  k1<-paso*S(xi,yi,zi,ti)
  l1<-paso*I(xi,yi,zi,ti)
  m1<-paso*R(xi,yi,zi,ti)
  
  k2<-paso*S(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  l2<-paso*I(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  m2<-paso*R(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  
  k3<-paso*S(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  l3<-paso*I(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  m3<-paso*R(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  
  k4<-paso*S(xi+k3,yi+l3,zi+m3,ti+paso)
  l4<-paso*I(xi+k3,yi+l3,zi+m3,ti+paso)
  m4<-paso*R(xi+k3,yi+l3,zi+m3,ti+paso)
  
  vS2[p]<-xi+(1/6)*(k1+2*k2+2*k3+k4)
  vI2[p]<-yi+(1/6)*(l1+2*l2+2*l3+l4)
  vR2[p]<-zi+(1/6)*(m1+2*m2+2*m3+m4)
  
  p<-p+1
}

plot(vS2, type="l", col="blue",axes=F,xlab = "Dias",ylab="Porcentaje")
par(new=TRUE)
plot(vI2, type="l",col="red",axes=F,xlab = "Dias",ylab="Porcentaje")
par(new=TRUE)
plot(vR2, type="l",col="black",xlab = "Dias",ylab="Porcentaje")
title(main="Valores reales contagio Covid-19 SantaMarta")

legend(x = "right", legend = c("Susceptibles", "Infectados", "Recuperados"), fill = c("blue", "red","black"),box.lty=0)

#-------------Punto 10
# Tabla de errores absolutos medios y errores relativos

MatrizError<-matrix(0:0, nrow=10, ncol=12)
MatrizError<-data.frame(MatrizError)
colnames(MatrizError)<-c("SusceptibleReal","SusceptibleAprox", "ErrorrelativoS","ErrorAbsolutoS",
      "InfectadosReal", "InfectadosAprox", "ErrorrelativoI","ErrorAbsolutoI",
      "RecuperadosReal","RecuperadosAprox", "ErrorrelativoR", "ErrorAbsolutoR")

MatrizError$SusceptibleReal<-vS2[1:10]
MatrizError$SusceptibleAprox<-vS[1:10]
MatrizError$InfectadosReal<-vI2[1:10]
MatrizError$InfectadosAprox<-vI[1:10]
MatrizError$RecuperadosReal<-vR2[1:10]
MatrizError$RecuperadosAprox<-vR[1:10]

for (i in 1:10){ 
  MatrizError$ErrorAbsolutoS[i]<-abs(vS[i]-vS2[i])
  MatrizError$ErrorRelativoS[i]<-MatrizError$ErrorrelativoS[i]/vS[i]
  MatrizError$ErrorAbsolutoI[i]<-abs(vI[i]-vI2[i])
  MatrizError$ErrorRelativoI[i]<-MatrizError$ErrorrelativoI[i]/vI[i]
  MatrizError$ErrorAbsolutoR[i]<-abs(vR[i]-vR2[i])
  MatrizError$ErrorRelativoR[i]<-MatrizError$ErrorrelativoR[i]/vR[i]
}

View(MatrizError)














