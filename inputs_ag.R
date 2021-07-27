library(rio)
library(plyr)
library(dplyr)
library(stringi)

#Ventas de insumos, (Produccion+Importacion-Exportacion) datos de las estadísticas de ingredientes activos agrícolas Colombia del ICA (1)

Cons_ins<-read.csv("Cons_ins_ag.csv", h=T,stringsAsFactors=FALSE, fileEncoding="latin1")
Cons_ins$kg.Lt<-ifelse(Cons_ins$kg.Lt<0,0,Cons_ins$kg.Lt)
temp<-list()
for (i in unique(Cons_ins$Accion)){
  temp[[i]]<-subset(Cons_ins,Accion==i)
  temp[[i]]$Perc<-temp[[i]]$kg.Lt/sum(temp[[i]]$kg.Lt)*100
}
Cons_ins<-do.call("rbind",temp);names(Cons_ins)[names(Cons_ins)=="Ing_activo"] <-"INGREDIENTE.ACTIVO"
Cons_ins<-na.omit(Cons_ins)

#Compilacion bases de datos ICA, SIPSA y consumo de insumos en Colombia
RegistroICA<-read.csv("Registro_ICA2020.csv", h=T, stringsAsFactors=FALSE, fileEncoding="latin1") #base de datos de insecticidas, fungicidas registrados por cultivo (2)
RegistroICA<-RegistroICA[,-c(1:3,5,7:11,13)];names(RegistroICA)[1]<-"Producto"
RegistroICA<-subset(RegistroICA,Producto!= "")
RegistroICA$Producto<-gsub( "®", " ", as.character(RegistroICA$Producto), 1)
RegistroICA$Producto<-sapply(strsplit(as.character(RegistroICA$Producto), " "), "[[",1)
#Costos de insumos y servicios SIPSA mas actual (3)
data_list <- import_list("Anexos_Insumos_dic_2020.xlsx")
index<-na.omit(data_list[[1]][,-1]);colnames(index)<-c("Id","Cont")
data_list<-data_list[c(3:18)]
data_list_ag<-data_list[grepl("1.",names(data_list))]
data_list_pec<-data_list[grepl("2.",names(data_list))]
data_list_other<-data_list[grepl("3.",names(data_list))]
rem_last_col<-function(x){
  x[,-ncol(x)]
   }
insumos<-list(data_list_ag,data_list_pec,data_list_other)
for (j in 1:length(insumos)){
insumos[[j]]<-lapply(insumos[[j]],FUN=rem_last_col)
insumos[[j]]<-lapply(insumos[[j]],FUN=na.omit)
data_list2<-list()
for (i in 1:length(insumos[[j]])){
  temp<-insumos[[j]][[i]]
  names(temp)=temp[1,]
  names(temp)[names(temp)=="Nombre del producto"] <-"Producto"
  temp<-temp[-1,]
  if("Nombre departamento" %in% names(temp)){
  temp$`Nombre departamento`<-stri_trans_general(temp$'Nombre departamento',id="Latin-ASCII")
  temp$`Nombre departamento`[temp$'Nombre departamento'=="Norte de Santander"]<-"N de Santander"
  }
  insumos[[j]][[i]]<-temp
  }
}

##Insumos agricolas
data_list_ag<-insumos[[1]]
for (i in 1:length(data_list_ag)){
  data_list_ag[[i]]$Producto<-gsub( "®", " ", as.character(data_list_ag[[i]]$Producto), 1)
  data_list_ag[[i]]$Producto<-sapply(strsplit(as.character(data_list_ag[[i]]$Producto), " "), "[[",1)
  data_list_ag[[i]]$Presentacion1<-sapply(strsplit(as.character(data_list_ag[[i]]$'Presentación del producto'), " "), "[[",1)
  data_list_ag[[i]]$Presentacion2<-sapply(strsplit(as.character(data_list_ag[[i]]$'Presentación del producto'), " "), "[[",2)
  data_list_ag[[i]]$prec.kg.lt=0
  for (j in 1:nrow(data_list_ag[[i]])){
  data_list_ag[[i]][j,]$prec.kg.lt<-ifelse(data_list_ag[[i]][j,]$Presentacion2=="gramos"|data_list_ag[[i]][j,]$Presentacion2=="centímetros",(as.numeric(data_list_ag[[i]][j,]$`Precio promedio de diciembre de 2020`)/as.numeric(data_list_ag[[i]][j,]$Presentacion1))*1000,
                                       as.numeric(data_list_ag[[i]][j,]$`Precio promedio de diciembre de 2020`)/as.numeric(data_list_ag[[i]][j,]$Presentacion1))
}
  data_list_ag[[i]]$Producto<-toupper(data_list_ag[[i]]$Producto)
}
RegistroICA$CULTIVO<-stri_trans_general(RegistroICA$CULTIVO, id = "Title")
RegistroICA$CULTIVO<-stri_trans_general(RegistroICA$CULTIVO, id = "Latin-ASCII")
data_list_ag<-lapply(data_list_ag,join,RegistroICA,by="Producto")
data_list_ag<-lapply(data_list_ag,join,Cons_ins,by="INGREDIENTE.ACTIVO")


#Fuentes 
#1. https://www.ica.gov.co/getattachment/Areas/Agricola/Servicios/Regulacion-y-Control-de-Plaguicidas-Quimicos/Estadisticas/ESTADISTICAS-PLAGUICIDAS-2019-1.pdf.aspx?lang=es-CO
#2. https://www.ica.gov.co/getdoc/d3612ebf-a5a6-4702-8d4b-8427c1cdaeb1/registros-nacionales-pqua-15-04-09.aspx
#3. https://www.dane.gov.co/index.php/estadisticas-por-tema/agropecuario/sistema-de-informacion-de-precios-sipsa/componente-insumos-1/componente-insumos-historicos
