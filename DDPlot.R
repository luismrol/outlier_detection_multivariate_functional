##
##--------------A homogeneity test for multivariate functional data------------------
#Autor: Luis Miguel Roldan Alzate
#Asesor: Francisco Zuluaga
#Objetivo: Construir los códigos para el desarrollo del trabajo de grado en matemáticas
#aplicadas
###---------------------------Carga de paquetes-----------------------------
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("DepthProc")
#install.packages("devtools")
library(DepthProc)
library(sn)
library(mvtnorm)
library(devtools)
###Generar los datos para hacer el ejercicio: dos normales bivariadas de media cero, idependientes y homoscedásticas con varianza I y 4*I.
standard <- mvrnorm(100, c(0.5, 2), diag(2))
shift <- mvrnorm(100, c(0,0), diag(2))
###Generar dataframes con profundidades respecto de sí mismas, y del otro conjunto, ordenadas para graficación
standard1 <- cbind(standard,depthMah(standard, X=standard), depthMah(u=standard, X=shift), depthMah(standard, X=standard),depthMah(u=standard, X=shift), rep(T, 100))
shift1 <- cbind(shift, depthMah(u=shift, X=standard), depthMah(u=shift, X=shift),depthMah(u=shift, X=shift), depthMah(u=shift, X=standard),rep(F, 100))
##Juntarlas 
appended<-data.frame(rbind(standard1, shift1))

###Scatter con indicador de color por muestra
scatter_muestra<-ggplot(appended, aes(x=X1,y=X2, color=X7))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  xlab("'X1' distribution")+
  ylab("'X2' distribution")
scatter_muestra
##Scatter con indicador de color por depth intramuestra 
scatter_intradepth<-ggplot(appended, aes(x=X1,y=X2, color=X5))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  xlab("'X1' distribution")+
  ylab("'X2' distribution")
scatter_intradepth
##Scatter con indicador de color por depth intermuestra
scatter_interdepth<-ggplot(appended, aes(x=X1,y=X2, color=X6))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  xlab("'X1' distribution")+
  ylab("'X2' distribution")

scatter_interdepth
##scatter ddplot "hecho a mano"
scatter_ddplot<-ggplot(appended, aes(x=X3,y=X4, color=X7))+
  geom_point()+
  coord_cartesian(xlim=c(0,1), ylim=c(0,1))+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Depth w.r.t. 'X' distribution")+
  ylab("Depth w.r.t. 'Y' distribution")
scatter_ddplot