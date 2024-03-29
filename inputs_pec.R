source('Costos_ag.R', encoding = 'UTF-8')
library(plyr)
library(stringi)

#compilacion bases de datos ICA, SIPSA de insumos pecuarios en Colombia (1)
RegistroICAV<-read.csv("RegistroICA_Vet2020.csv", h=T)#, stringsAsFactors=FALSE, fileEncoding="latin1")
names(RegistroICAV)[1]<-"Producto";RegistroICAV<-RegistroICAV[c(1:3)]
RegistroICAV<-subset(RegistroICAV,Producto!= "")
RegistroICAV$Producto<-gsub( "®", " ", as.character(RegistroICAV$Producto), 1)
RegistroICAV$Producto<-sapply(strsplit(as.character(RegistroICAV$Producto), " "), "[[",1)

##Insumos pecuarios
data_list_pec<-insumos[[2]]
for (i in 1:length(data_list_pec)){
  data_list_pec[[i]]$Producto<-gsub( "®", " ", as.character(data_list_pec[[i]]$Producto), 1)
  data_list_pec[[i]]$Producto<-sapply(strsplit(as.character(data_list_pec[[i]]$Producto), " "), "[[",1)
  data_list_pec[[i]]$Presentacion1<-sapply(strsplit(as.character(data_list_pec[[i]]$'Presentación del producto'), " "), "[[",1)
  data_list_pec[[i]]$Presentacion2<-sapply(strsplit(as.character(data_list_pec[[i]]$'Presentación del producto'), " "), "[[",2)
  data_list_pec[[i]]$prec.dosis=0
  for (j in 1:nrow(data_list_pec[[i]])){
  data_list_pec[[i]][j,]$prec.dosis<-ifelse(data_list_pec[[i]][j,]$Presentacion2=="gramos"|data_list_pec[[i]][j,]$Presentacion2=="centímetros",(as.numeric(data_list_pec[[i]][j,]$Precio_actual)/as.numeric(data_list_pec[[i]][j,]$Presentacion1))*10,
                                                   ifelse(data_list_pec[[i]][j,]$Presentacion2=="M.U.I",(as.numeric(data_list_pec[[i]][j,]$Precio_actual)/as.numeric(data_list_pec[[i]][j,]$Presentacion1))*2,
                                       as.numeric(data_list_pec[[i]][j,]$Precio_actual)/as.numeric(data_list_pec[[i]][j,]$Presentacion1)/100))
  
}
  data_list_pec[[i]]$Producto<-toupper(data_list_pec[[i]]$Producto)
}
RegistroICAV$Linea<-stri_trans_general(RegistroICAV$Linea, id = "Latin-ASCII")
data_list_pec<-lapply(data_list_pec,join,RegistroICAV,by="Producto")


# 1.https://www.ica.gov.co/areas/pecuaria/servicios/regulacion-y-control-de-medicamentos-veterinarios/productos-veterinarios-registrados.aspx