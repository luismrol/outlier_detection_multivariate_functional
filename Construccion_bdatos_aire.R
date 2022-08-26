# LECTURA DE DATOS
# CALIDAD DEL AIRE
# LUIS MIGUEL ROLDAN

library(dplyr)
library(tidyr)
library(mtsdi)
library(xtable)

# FUNCIONES 

rollmean_faire<-function(data){
  data<-as.numeric(data)
  rows<-length(data)
  mean<-matrix(NA, rows, 1)
  for (i in 1:rows){
    mean[i]<-mean(c(data[i],data[i+1],data[i+2],data[i+3],data[i+4],data[i+5],data[i+6],data[i+7]))
  }
  return(mean)
}


# LECTURA DE LAS BASES DE DATOS
# READING DATASETS

temp = list.files(pattern="estacion*")
myfiles = lapply(temp, read.csv)
aire=rbind.data.frame(myfiles[[1]], myfiles[[2]])
aire=rbind.data.frame(aire, myfiles[[3]])
colnames(aire)[1]<-"Fecha_Hora"
aire=rbind.data.frame(aire, myfiles[[4]])
aire=rbind.data.frame(aire, myfiles[[5]])
aire=rbind.data.frame(aire, myfiles[[6]])
aire=rbind.data.frame(aire, myfiles[[7]])

aire2=rbind.data.frame(myfiles[[8]], myfiles[[9]])
aire2=rbind.data.frame(aire2, myfiles[[10]])
aire2=rbind.data.frame(aire2, myfiles[[11]])
aire2=rbind.data.frame(aire2, myfiles[[12]])
aire2=rbind.data.frame(aire2, myfiles[[13]])
aire2=rbind.data.frame(aire2, myfiles[[14]])
aire2=rbind.data.frame(aire2, myfiles[[15]])
aire2=rbind.data.frame(aire2, myfiles[[16]])
aire2=rbind.data.frame(aire2, myfiles[[17]])
aire2=rbind.data.frame(aire2, myfiles[[18]])
aire2=rbind.data.frame(aire2, myfiles[[19]])
aire2=rbind.data.frame(aire2, myfiles[[20]])
aire2=rbind.data.frame(aire2, myfiles[[21]])
aire2=rbind.data.frame(aire2, myfiles[[22]])

aire3=rbind.data.frame(myfiles[[23]], myfiles[[24]])
aire3=rbind.data.frame(aire3, myfiles[[25]])
colnames(aire3)[1]<-"Fecha_Hora"

aire3=rbind.data.frame(aire3, myfiles[[26]])
aire3=rbind.data.frame(aire3, myfiles[[27]])
aire3=rbind.data.frame(aire3, myfiles[[28]])
aire3=rbind.data.frame(aire3, myfiles[[29]])

aire4=rbind.data.frame(myfiles[[30]], myfiles[[31]])
aire4=rbind.data.frame(aire4, myfiles[[32]])
aire4=rbind.data.frame(aire4, myfiles[[33]])
aire4=rbind.data.frame(aire4, myfiles[[34]])
aire4=rbind.data.frame(aire4, myfiles[[35]])
aire4=rbind.data.frame(aire4, myfiles[[36]])
aire4=rbind.data.frame(aire4, myfiles[[37]])
aire4=rbind.data.frame(aire4, myfiles[[38]])
aire4=rbind.data.frame(aire4, myfiles[[39]])
aire4=rbind.data.frame(aire4, myfiles[[40]])
aire4=rbind.data.frame(aire4, myfiles[[41]])
aire4=rbind.data.frame(aire4, myfiles[[42]])
aire4=rbind.data.frame(aire4, myfiles[[43]])
aire4=rbind.data.frame(aire4, myfiles[[44]])

aire5=rbind.data.frame(myfiles[[45]], myfiles[[46]])
aire5=rbind.data.frame(aire5, myfiles[[47]])
colnames(aire5)[1]<-"Fecha_Hora"

aire5=rbind.data.frame(aire5, myfiles[[48]])
aire5=rbind.data.frame(aire5, myfiles[[49]])
aire5=rbind.data.frame(aire5, myfiles[[50]])
aire5=rbind.data.frame(aire5, myfiles[[51]])

aire6=rbind.data.frame(myfiles[[52]], myfiles[[53]])
aire6=rbind.data.frame(aire6, myfiles[[54]])
aire6=rbind.data.frame(aire6, myfiles[[55]])
aire6=rbind.data.frame(aire6, myfiles[[56]])
aire6=rbind.data.frame(aire6, myfiles[[57]])
aire6=rbind.data.frame(aire6, myfiles[[58]])
aire6=rbind.data.frame(aire6, myfiles[[59]])
aire6=rbind.data.frame(aire6, myfiles[[60]])
aire6=rbind.data.frame(aire6, myfiles[[61]])
aire6=rbind.data.frame(aire6, myfiles[[62]])
aire6=rbind.data.frame(aire6, myfiles[[63]])
aire6=rbind.data.frame(aire6, myfiles[[64]])
aire6=rbind.data.frame(aire6, myfiles[[65]])
colnames(myfiles[[66]])[1]<-"Fecha_Hora"
aire6=rbind.data.frame(aire6, myfiles[[66]])


aire7=rbind.data.frame(myfiles[[67]], myfiles[[68]])
aire7=rbind.data.frame(aire7, myfiles[[69]])
colnames(aire7)[1]<-"Fecha_Hora"

aire7=rbind.data.frame(aire7, myfiles[[70]])
aire7=rbind.data.frame(aire7, myfiles[[71]])
aire7=rbind.data.frame(aire7, myfiles[[72]])
aire7=rbind.data.frame(aire7, myfiles[[73]])

aire8=rbind.data.frame(myfiles[[74]], myfiles[[75]])
aire8=rbind.data.frame(aire8, myfiles[[76]])
aire8=rbind.data.frame(aire8, myfiles[[77]])
aire8=rbind.data.frame(aire8, myfiles[[78]])
aire8=rbind.data.frame(aire8, myfiles[[79]])
aire8=rbind.data.frame(aire8, myfiles[[80]])
aire8=rbind.data.frame(aire8, myfiles[[81]])
aire8=rbind.data.frame(aire8, myfiles[[82]])
aire8=rbind.data.frame(aire8, myfiles[[83]])
aire8=rbind.data.frame(aire8, myfiles[[84]])
aire8=rbind.data.frame(aire8, myfiles[[85]])
aire8=rbind.data.frame(aire8, myfiles[[86]])
aire8=rbind.data.frame(aire8, myfiles[[87]])
aire8=rbind.data.frame(aire8, myfiles[[88]])

names(aire2)=names(aire)
names(aire2)=names(aire)
names(aire3)=names(aire)
names(aire4)=names(aire)
names(aire5)=names(aire)
names(aire6)=names(aire)
names(aire7)=names(aire)
names(aire8)=names(aire)
aire=rbind.data.frame(aire, aire2)
aire<-rbind.data.frame(aire, aire3)
aire<-rbind.data.frame(aire, aire4)
aire<-rbind.data.frame(aire, aire5)
aire<-rbind.data.frame(aire, aire6)
aire<-rbind.data.frame(aire, aire7)
aire<-rbind.data.frame(aire, aire8)

# EDICION DE LAS BASES DE DATOS PARA PROCESAMIENTO
# EDITING DATABASES FOR PROCESSING

aire_corto=dplyr::select(aire, Fecha_Hora, codigoSerial, pm25, calidad_pm25)

aire_corto$Fecha=as.Date(t(as.data.frame(strsplit(aire_corto$Fecha_Hora," ")))[,1])
aire_corto$Fecha_Hora=as.POSIXct(aire_corto$Fecha_Hora)
aire_corto$hora=as.numeric(as.character(format(aire_corto$Fecha_Hora, "%H")))

# GENERAR LA BASE PARA IDENTIFICAR LOS DATOS FALTANTES
fecha<-unique(dplyr::select(aire_corto, Fecha))
hora<-unique(dplyr::select(aire_corto, hora))
fechahora<-merge(fecha, hora)

# SELECCIONAR LAS VARIABLES PARA ANÁLISIS Y APLICAR CRITERIOS DE CALIDAD

aire_cortototal<-dplyr::select(aire_corto, Fecha_Hora, Fecha, codigoSerial, pm25, calidad_pm25, hora)
aire_cortototal$pm25[aire_cortototal$calidad_pm25>2.5]<-NA
aire_cortototal$pm25[aire_cortototal$pm25<0]<-NA
aire_cortototal<-dplyr::select(aire_cortototal, Fecha_Hora, Fecha, codigoSerial, pm25, hora)
missings<-subset(aire_cortototal, is.na(aire_cortototal$pm25))

missings_table<-missings %>% group_by(Fecha, codigoSerial) %>% tally()
missings_table<-replace_na(spread(missings_table, codigoSerial, n), list(n=0, '12'=0, '28'=0, '48'=0, '80'=0))
names(missings_table)<-c("Fecha", "e12", "e28", "e48", "e80")
missings_table[2:5]<-apply(missings_table[2:5],2, function(x) x/24)


missingsplot<-missings %>% group_by(Fecha, codigoSerial) %>% tally() %>% group_by(n, codigoSerial) %>% tally() 
missingsplot<-replace_na(spread(missingsplot, codigoSerial, nn), list(n=0, '12'=0, '28'=0, '48'=0, '80'=0))
names(missingsplot)<-c("Number of missing hours in a day", "TC", "CJUS", "TS", "VH")
missingsplot<-apply(missingsplot, 2, as.integer)
missingsplottotal<-colSums(missingsplot)
missingsplottotal[1]<-"total"
missingsplot<-rbind(missingsplot, missingsplottotal)

print(xtable(missingsplot, type = "latex"), file="tabla_missings.tex")


# REDUCIR LAS BASES DE DATOS A LA DIMENSIÓN ORIGINAL

aire_cortototal<-dplyr::select(aire_cortototal, Fecha_Hora, Fecha, codigoSerial, pm25, hora)
aire_cortototal<-pivot_wider(aire_cortototal, names_from = codigoSerial, values_from=pm25)

print(xtable(cor(drop_na(aire_cortototal[,4:7]), method="pearson"), type="latex"), file="tabla_correlacion.tex")

# IDENTIFICAR LOS NA QUE PUDIERON QUEDAR FALTANDO
names(aire_cortototal)<-c("Fecha_Hora", "Fecha","hora", "e12", "e28","e48","e80")
# IMPUTAR MEDIANTE ALGORITMO EM
filaire<-mnimput(~ e12 + e28  + e48 + e80, aire_cortototal)

# LLENAR LA BASE CON LOS DATOS IMPUTADOS
aire_cortototal$e12<-filaire$filled.dataset$e12
aire_cortototal$e28<-filaire$filled.dataset$e28
aire_cortototal$e48<-filaire$filled.dataset$e48
aire_cortototal$e80<-filaire$filled.dataset$e80



names(aire_cortototal)

pm_12<-spread(dplyr::select(aire_cortototal, Fecha, hora, e12), hora, e12)
pm_28<-spread(dplyr::select(aire_cortototal, Fecha, hora, e28), hora, e28)
pm_48<-spread(dplyr::select(aire_cortototal, Fecha, hora, e48), hora, e48)
pm_80<-spread(dplyr::select(aire_cortototal, Fecha, hora, e80), hora, e80)


plot1_pm_12<-pivot_longer(merge(pm_12, dplyr::select(missings_table, Fecha, e12)), c(2:25), names_to="hora")
plot1_pm_28<-pivot_longer(merge(pm_28, dplyr::select(missings_table, Fecha, e28)), c(2:25), names_to="hora")
plot1_pm_48<-pivot_longer(merge(pm_48, dplyr::select(missings_table, Fecha, e48)), c(2:25), names_to="hora")
plot1_pm_80<-pivot_longer(merge(pm_80, dplyr::select(missings_table, Fecha, e80)), c(2:25), names_to="hora")


pm_12$mean<-as.numeric(rowMeans(pm_12[,2:25]))
pm_12$mean7<-rollmean_faire(pm_12$mean)
pm_12$critical<-pm_12$mean<37
pm_12$critical7<-pm_12$mean7<37



pm_28$mean<-as.numeric(rowMeans(pm_28[,2:25]))
pm_28$mean7<-rollmean_faire(pm_28$mean)
pm_28$critical<-pm_28$mean<25
pm_28$critical7<-pm_28$mean7<25

pm_48$mean<-as.numeric(rowMeans(pm_48[,2:25]))
pm_48$mean7<-rollmean_faire(pm_48$mean)
pm_48$critical<-pm_48$mean<25
pm_48$critical7<-pm_48$mean7<25



pm_80$mean<-as.numeric(rowMeans(pm_80[,2:25]))
pm_80$mean7<-rollmean_faire(pm_80$mean)
pm_80$critical<-pm_80$mean<10
pm_80$critical7<-pm_80$mean7<10


critical7<-rowSums(cbind(pm_12$critical7, pm_28$critical7, pm_48$critical7, pm_80$critical7))>0
pm_12$critical7<-critical7
pm_28$critical7<-critical7
pm_48$critical7<-critical7
pm_80$critical7<-critical7
graf_pm80<-pivot_longer(pm_80, c(2:25), names_to="hora")
graf_pm48<-pivot_longer(pm_48, c(2:25), names_to="hora")
graf_pm12<-pivot_longer(pm_12, c(2:25), names_to="hora")
graf_pm28<-pivot_longer(pm_28, c(2:25), names_to="hora")



# BASE POR ESTACIÓN



library(ggplot2)

plot_pm12<-ggplot(data=plot1_pm_12, aes(x=as.numeric(hora), y=value, group=Fecha, col=e12))+geom_line()+
  scale_colour_gradient2(low="blue", high="red", mid="black", midpoint = 0.4)+ggtitle("PM2.5 at CEN-TRAF")+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="% points missing")

plot_pm28<-ggplot(data=plot1_pm_28, aes(x=as.numeric(hora), y=value, group=Fecha, col=e28))+geom_line()+
  scale_colour_gradient2(low="blue", high="red", mid="black", midpoint = 0.4)+ggtitle("PM2.5 at ITA-CJUS")+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="% points missing")

plot_pm48<-ggplot(data=plot1_pm_48, aes(x=as.numeric(hora), y=value, group=Fecha, col=e48))+geom_line()+
  scale_colour_gradient2(low="blue", high="red", mid="black", midpoint = 0.4)+ggtitle("PM2.5 at SUR-TRAF")+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="% points missing")

plot_pm80<-ggplot(data=plot1_pm_80, aes(x=as.numeric(hora), y=value, group=Fecha, col=e80))+geom_line()+
  scale_colour_gradient2(low="blue", high="red", mid="black", midpoint = 0.4)+ggtitle("PM2.5 at MED-VILL")+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="% points missing")


plot2_pm12<-ggplot(data=subset(graf_pm12, !is.na(critical7)), aes(x=as.numeric(hora), y=value, group=Fecha, col=critical7))+geom_line()+
  ggtitle("PM2.5 at CEN-TRAF with classification label")+scale_colour_manual(values=c("TRUE"="midnightblue", "FALSE"="gray67"))+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="Critical")

plot2_pm28<-ggplot(data=subset(graf_pm28, !is.na(critical7)), aes(x=as.numeric(hora), y=value, group=Fecha, col=critical7))+geom_line()+
  ggtitle("PM2.5 at ITA-CJUS with classification label")+scale_colour_manual(values=c("TRUE"="midnightblue", "FALSE"="gray67"))+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="Critical")

plot2_pm48<-ggplot(data=subset(graf_pm48, !is.na(critical7)), aes(x=as.numeric(hora), y=value, group=Fecha, col=critical7))+geom_line()+
  ggtitle("PM2.5 at SUR-TRAF with classification label")+scale_colour_manual(values=c("TRUE"="midnightblue", "FALSE"="gray67"))+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="Critical")

plot2_pm80<-ggplot(data=subset(graf_pm80, !is.na(critical7)), aes(x=as.numeric(hora), y=value, group=Fecha, col=critical7))+geom_line()+
  ggtitle("PM2.5 at MED-VILL with classification label")+scale_colour_manual(values=c("TRUE"="midnightblue", "FALSE"="gray67"))+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="Critical")

plot2_pm12




order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
graf_noout1<-cbind(order1_200, as.data.frame(data_noout[[1]]))
names(graf_noout1)<-c("N", 1:200)
graf_noout1<-pivot_longer(graf_noout1,c(2:201) )

graf_noout2<-cbind(order1_200, as.data.frame(data_noout[[2]]))
names(graf_noout2)<-c("N", 1:200)
graf_noout2<-pivot_longer(graf_noout2,c(2:201) )

disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outmag1<-cbind(order1_200, as.data.frame(data_out_mag80[[1]]))
names(graf_outmag1)<-c("col", "N", 1:200)
graf_outmag1<-pivot_longer(graf_outmag1,c(3:202) )

order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outmag2<-cbind(order1_200, as.data.frame(data_out_mag80[[2]]))
names(graf_outmag2)<-c("col", "N", 1:200)
graf_outmag2<-pivot_longer(graf_outmag2,c(3:202) )


order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outshape1<-cbind(order1_200, as.data.frame(data_out_shape80[[1]]))
names(graf_outshape1)<-c("col", "N", 1:200)
graf_outshape1<-pivot_longer(graf_outshape1,c(3:202) )

order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outshape2<-cbind(order1_200, as.data.frame(data_out_shape80[[2]]))
names(graf_outshape2)<-c("col", "N", 1:200)
graf_outshape2<-pivot_longer(graf_outshape2,c(3:202) )


order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outcov1<-cbind(order1_200, as.data.frame(data_out_cov80[[1]]))
names(graf_outcov1)<-c("col", "N", 1:200)
graf_outcov1<-pivot_longer(graf_outcov1,c(3:202) )


order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outcov2<-cbind(order1_200, as.data.frame(data_out_cov80[[2]]))
names(graf_outcov2)<-c("col", "N", 1:200)
graf_outcov2<-pivot_longer(graf_outcov2,c(3:202) )


order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outmix1<-cbind(order1_200, as.data.frame(data_out_mix80[[2]]))
names(graf_outmix1)<-c("col", "N", 1:200)
graf_outmix1<-pivot_longer(graf_outmix1,c(3:202) )


order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outmix2<-cbind(order1_200, as.data.frame(data_out_mix80[[2]]))
names(graf_outmix2)<-c("col", "N", 1:200)
graf_outmix2<-pivot_longer(graf_outmix2,c(3:202) )


order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outiso1<-cbind(order1_200, as.data.frame(data_out_iso80[[2]]))
names(graf_outiso1)<-c("col", "N", 1:200)
graf_outiso1<-pivot_longer(graf_outiso1,c(3:202) )


order1_200<-as.data.frame(matrix(data=(1:200), nrow=200, ncol=1))
names(order1_200)<-"N"
disc_1_200<-matrix(0,200,1)
disc_1_200[161:200]<-1
order1_200<-cbind(disc_1_200, order1_200)
graf_outiso2<-cbind(order1_200, as.data.frame(data_out_iso80[[2]]))
names(graf_outiso2)<-c("col", "N", 1:200)
graf_outiso2<-pivot_longer(graf_outiso2,c(3:202) )



plot_noout1<-ggplot(data=graf_noout1, aes(x=as.numeric(name), y=value, group=N))+geom_line(col="blue")+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude")

plot_noout2<-ggplot(data=graf_noout2, aes(x=as.numeric(name), y=value, group=N))+geom_line(col="blue")+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude")


plot_outmag1<-ggplot(data=graf_outmag1, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude",col="outliers (1)")

plot_outmag2<-ggplot(data=graf_outmag2, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude", col="outliers (1)")

plot_outshape1<-ggplot(data=graf_outshape1, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude",col="outliers (1)")

plot_outshape2<-ggplot(data=graf_outshape2, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude", col="outliers (1)")

plot_outcov1<-ggplot(data=graf_outcov1, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude",col="outliers (1)")

plot_outcov2<-ggplot(data=graf_outcov2, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude", col="outliers (1)")

plot_outmix1<-ggplot(data=graf_outmix1, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude",col="outliers (1)")

plot_outmix2<-ggplot(data=graf_outmix2, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude", col="outliers (1)")

plot_outiso1<-ggplot(data=graf_outiso1, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude",col="outliers (1)")

plot_outiso2<-ggplot(data=graf_outiso2, aes(x=as.numeric(name), y=value, group=N, col=col))+geom_line()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Domain", y="Magnitude", col="outliers (1)")

plot_outmag1
plot_noout
library(gridExtra)


dev.new()
gridExtra::grid.arrange(plot_outmag1, plot_outmag2, ncol=2)
gridExtra::grid.arrange(plot_outcov1, plot_outcov2, ncol=2)
gridExtra::grid.arrange(plot_outiso1, plot_outiso2, ncol=2)
gridExtra::grid.arrange(plot_outmix1, plot_outmix2, ncol=2)
gridExtra::grid.arrange(plot_noout1, plot_noout2, ncol=2)



gridExtra::grid.arrange(plot_outshape1, plot_outshape2, ncol=2)


png("plot_noout.png")

print(gridExtra::grid.arrange(plot_noout1, plot_noout2, ncol=2))
dev.off()


gridExtra::grid.arrange(plot_shape1, plot_shape2, ncol=2)
ggsave("plot_shape.png")
dev.off()

gridExtra::grid.arrange(plot_pm12, plot_pm28, plot_pm48, plot_pm80)
gridExtra::grid.arrange(plot2_pm12, plot2_pm28, plot2_pm48, plot2_pm80)


a<-gridExtra::grid.arrange(plot_outmag1, plot_outmag2, ncol=2)
gridExtra::grid.arrange(plot_outshape1, plot_outshape2, ncol=2)


### Eliminación de atípicos

mfdata_aire<-roahd::mfData(c(1:24), list(pm12=pm_12[,2:25], pm28=pm_28[,2:25], pm48=pm_48[,2:25], pm80=pm_80[,2:25]))

out<-as.vector(generic_crit_outliers(mfdata_aire, "hmodal"))
out<-as.data.frame(out)
n<-as.data.frame(1:670)
out$out1<-"OUT"
names(n)<-"n"
names(out)<-c("n", "out")
out2<-merge(out, n, all.y=T)
out2$out<-replace_na(out2$out, "NO OUT")

pm_12$out<-out2$out
pm_28$out<-out2$out
pm_48$out<-out2$out
pm_80$out<-out2$out

graf_pm80<-pivot_longer(pm_80, c(2:25), names_to="hora")
graf_pm48<-pivot_longer(pm_48, c(2:25), names_to="hora")
graf_pm12<-pivot_longer(pm_12, c(2:25), names_to="hora")
graf_pm28<-pivot_longer(pm_28, c(2:25), names_to="hora")

plot3_pm12<-ggplot(data=subset(graf_pm12, !is.na(out)), aes(x=as.numeric(hora), y=value, group=Fecha, col=out))+geom_line()+
  ggtitle("PM2.5 at CEN-TRAF with outlier label")+scale_colour_manual(values=c("OUT"="midnightblue", "NO OUT"="gray67"))+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="Outlier")

plot3_pm28<-ggplot(data=subset(graf_pm28, !is.na(out)), aes(x=as.numeric(hora), y=value, group=Fecha, col=out))+geom_line()+
  ggtitle("PM2.5 at ITA-CJUS with outlier label")+scale_colour_manual(values=c("OUT"="midnightblue", "NO OUT"="gray67"))+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="Outlier")

plot3_pm48<-ggplot(data=subset(graf_pm48, !is.na(out)), aes(x=as.numeric(hora), y=value, group=Fecha, col=out))+geom_line()+
  ggtitle("PM2.5 at SUR-TRAF with outlier label")+scale_colour_manual(values=c("OUT"="midnightblue", "NO OUT"="gray67"))+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="Outlier")

plot3_pm80<-ggplot(data=subset(graf_pm80, !is.na(out)), aes(x=as.numeric(hora), y=value, group=Fecha, col=out))+geom_line()+
  ggtitle("PM2.5 at MED-VILL with outlier label")+scale_colour_manual(values=c("OUT"="midnightblue", "NO OUT"="gray67"))+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())+labs(x="Hour", y="Concentration", col="Outlier")

plot3_pm80

gridExtra::grid.arrange(plot3_pm12, plot3_pm28, plot3_pm48, plot3_pm80)


generic_crit_outliers<-function(mfdata, depth, Nboot=50, sizeboot=30, treshold=0.01){
  if (depth=="hmodal"){
    depths_vector<-as.vector(fda.usc::depth.modep(list(one=fda.usc::fdata(as.matrix(mfdata$fDList$pm12$values)),two=fda.usc::fdata(as.matrix(mfdata$fDList$pm28$values)),three=fda.usc::fdata(as.matrix(mfdata$fDList$pm48$values)),four=fda.usc::fdata(as.matrix(mfdata$fDList$pm80$values))))$dep)
  }
  a<-list()
  c<-list()
  weight<-c()
  for (i in 1:length(depths_vector)){
    weight[i]<-(depths_vector[i]-min(depths_vector))/(max(depths_vector)-min(depths_vector))
  }
  for( i in 1:Nboot){
    a[[i]]<-sample(depths_vector, sizeboot, replace=T, prob=weight)
    c[[i]]<-quantile(a[[i]], treshold)
  }
  c<-unlist(c)
  d<-median(unlist(c))
  f<-which(depths_vector<d) 
  return(f)
}


