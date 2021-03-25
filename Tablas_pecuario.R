source("inputs_ag.R",keep.source = TRUE)
source("Tablas_ag.R",keep.source = TRUE)
library(rio)
library(DescTools)
#Ingresando las unidades por item y calculando los valores
Lineas<-c("Carne_bov","Leche_bov")
Departamentos<-c("Caqueta","Narino","Putumayo","Cauca","Bolivar","Cordoba","Magdalena","Sucre","Atlantico","Guajira","Cesar","Santander","N de Santander","Tolima","Huila","Caldas","Quindio","Risaralda","Meta","Casanare","Vichada","Arauca","Boyaca","Cundinamarca","Antioquia","Valle del Cauca","Amazonas","Choco","San Andres","Guaviare","Vaupes","Guainia")
unitsV<-read.csv("../costos database/UnitsV.csv", h=T)
UnitsV<-import_list("C:/Users/hac809/Desktop/FAO/costos database/Tabla_unid_pec.xlsx")
for(i in 1:length(Units)){Units[[i]][,2]<-as.numeric(Units[[i]][,2])}

#Especies
a=unique(insumos[[3]][[2]]$`Nombre de la especie productiva`)[c(6:24)]
Especies<-insumos[[3]][[2]][insumos[[3]][[2]]$`Nombre de la especie productiva`%in% a,c(2,5,6,8)];names(Especies)=c("Departamento","Especie","Presentacion","Precio")
Especies$Precio<-ifelse(Especies$Presentacion=="kilogramo",as.numeric(Especies$Precio)*350,as.numeric(Especies$Precio))
ValorEs<-Valor
for (i in 1:nrow(ValorEs)){
  temp<-subset(Especies,Especies[1]==as.character(ValorEs$Departamento[i]))
  av<-mean(as.numeric(temp[,4]))
  ValorEs$Valor[i]<-av
}


#Usar Valor *3 para Jornales

#Enmiendas, Herbicidas, Insecticidas
#usar ValorE para enmiendas

InsecV<-data_list_ag[[5]][,c(2,11,12,13,14,17)]
InsecV<-subset(InsecV,Presentacion2=="litro"|Presentacion2=="litros"|Presentacion2=="kilogramo")
ValorInV<-Valor[1]
for (j in 1:nrow(ValorInV)){
  temp<-subset(InsecV,InsecV[1]==as.character(ValorInV$Departamento[j]))
  av<-mean(as.numeric(temp[,3],na.rm = TRUE))
  ValorInV$Valor[j]<-av
}

HerbiV<-data_list_ag[[4]][,c(2,11,12,13,14,17)]
HerbiV<-subset(HerbiV,Presentacion2=="litro"|Presentacion2=="litros"|Presentacion2=="kilogramo")
ValorHeV<-Valor[1]
for (j in 1:nrow(ValorHeV)){
  temp<-subset(HerbiV,HerbiV[1]==as.character(ValorHeV$Departamento[j]))
  av<-mean(as.numeric(temp[,3],na.rm = TRUE))
  ValorHeV$Valor[j]<-av
}


#Semilla
ValorSem<-Valor[1]
ValorSem$Valor<-4000

#Sal
ValorSal<-Valor[1]
ValorSal$Valor<-2000

#Hormonas
Hormo<-data_list_pec[[4]][data_list_pec[[4]]$Linea %like% "%ovino%",c(2,8,10,11,12,14)];names(Hormo)[c(1,2)]<-c("Departamentos","Precio")
ValorHor<-Valor[1]
for (j in 1:nrow(ValorHor)){
  temp<-subset(Hormo,Hormo[1]==as.character(ValorHor$Departamento[j]))
  av<-mean(as.numeric(temp[,2],na.rm = TRUE))/4
  ValorHor$Valor[j]<-av
}
#Medicamentos
Med<-data_list_pec[[6]][data_list_pec[[6]]$Linea %like% "%ovino%",c(2,8,10,11,12,14)];names(Med)[c(1,2)]<-c("Departamentos","Precio")
ValorMed<-Valor[1]
for (j in 1:nrow(ValorMed)){
  temp<-subset(Med,Med[1]==as.character(ValorMed$Departamento[j]))
  av<-mean(as.numeric(temp[,2],na.rm = TRUE))/5
  ValorMed$Valor[j]<-av
}

#Mecanizacion
ValorMec<-Valor[1]
ValorMec$Valor<-90000

ValV<-list(ValorEs,Valor,Valor, Valor, ValorE,ValorInV,ValorHeV,ValorSem,ValorSal,ValorHor,ValorMed,ValorMec)

for(i in 1:length(ValV)){
  ValV[[i]]<-cbind(ValV[[i]][1],rep(ValV[[i]][2],times=2))
  colnames(ValV[[i]])<-colnames(unitsV)
}

for (i in 1:length(ValV)){
  for (j in 2:ncol(ValV[[i]])){
ValV[[i]][,j]<-ifelse(is.na(ValV[[i]][,j]), mean((ValV[[i]][,j]),na.rm=TRUE),ValV[[i]][,j])
UnitsV[[i]][,j]<-ifelse(is.na(UnitsV[[i]][,j]), mean((UnitsV[[i]][,j]),na.rm=TRUE),UnitsV[[i]][,j])
  }}

