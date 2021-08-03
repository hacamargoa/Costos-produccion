source('Tablas_agricola.R', encoding = 'UTF-8')

library(knitr)
library(kableExtra)
#compilación de tablas de costos por departamento y linea productiva en la lista Deptos 
Tabla_costos_ag<-read.csv("Tabla_costos.csv", h=T,stringsAsFactors=FALSE, fileEncoding="latin1")
Arriendos<-read.csv("Arriendos.csv", h=T, stringsAsFactors=FALSE, fileEncoding="latin1") #Precios Sipsa (1)
Maquina<-read.csv("Maquina.csv", h=T, stringsAsFactors=FALSE, fileEncoding="latin1") #Precios sipsa (1)
Perc_propag<-read.csv("Perc_propag.csv", h=T, stringsAsFactors=FALSE, fileEncoding="latin1") #Creado de las información secundaria
Deptos<-list()


for(j in Departamentos){
  Cultiv<-list()
  Val_Gasol<-8500
  Val_Aceite<-15000
  for (i in 1:length(Cultivos)){
    temp<-Tabla_costos_ag
    for (f in 1:15){
      temp$CANTIDAD[f]<-as.numeric(Units[[f]][Units[[f]]$Departamento==j,Cultivos[i]])
      temp$Valor_Uni[f]<-Val3[[f]][Val3[[f]]$Departamento==j,Cultivos[i]]
      f=f+1
    }
    
    temp[18,3]<-sum(temp[9,3],temp[10,3])/50;temp[18,4]<-4000
    temp[22,3]<-1;temp[22,4]=Arriendos[Arriendos$Departamento==j,2]
    temp[16,3]<-ifelse(i %in% c(2,6,10,13,15,18,21,23,24),0,5)
    temp[16,4]<-Maquina[Maquina$Departamento==j,2]
    temp[20,3]<-1;temp[20,c(4)]<-500000
    temp$Costo<-as.numeric(temp$CANTIDAD)*as.numeric(temp$Valor_Uni)
    temp[19,1]<-ifelse(i==match("Madera_sos",Cultivos)|i==match("Madera_cos",Cultivos),"Mat. Equip. Gasol. y Aceite",temp[19,1]);temp[19,3]<-1
    temp[19,c(4,5)]<-ifelse(i==match("Madera_sos",Cultivos),sum(temp[,5])*0.1+35.5*Val_Gasol+2.7*Val_Aceite,
                         ifelse(i==match("Madera_cos",Cultivos),sum(temp[,5])*0.1+71.15*Val_Gasol+5.35*Val_Aceite,sum(temp[,5])*0.06))
    temp[21,3]<-1;temp[21,c(4,5)]<-sum(temp[,5])*0.06
    temp[17,3]<-1;temp[17,c(4,5)]<-sum(temp[,5])*Perc_propag[Perc_propag$Cultivo==Cultivos[i],2]
    Cultiv[[i]]<-temp
    }
  Deptos[[j]]<-Cultiv;names(Deptos[[j]])<-Cultivos
}
#names(Deptos)<-dep
#Calculo de tablas de costos para la creacion de mapas
cost<-data.frame(matrix(NA,nrow=32,ncol=length(Cultivos)))
codesDepto<-read.csv("codedepto.csv",h=T, stringsAsFactors=FALSE, fileEncoding="latin1") # Codigos de colmap library
codesDepto$id_depto<-sprintf("%02d",codesDepto$id_depto)
colnames(cost)<-Cultivos;rownames(cost)<-c(Departamentos)
Cost_total<-cost
Cost_MO<-cost
Cost_insum<-cost 
Cost_otros<-cost
Cost_fijos<-cost
Cost_variables<-cost
for(i in Departamentos){
  
  for (j in Cultivos){
    Cost_total[i,j]<-ifelse(sum(Deptos[[i]][[j]][c(1:8),5])==0,0,sum(Deptos[[i]][[j]][,5]))
    Cost_MO[i,j]<-ifelse(sum(Deptos[[i]][[j]][c(1:8),5])==0,0,sum(Deptos[[i]][[j]][c(1:8),5]))
    Cost_insum[i,j]<-ifelse(sum(Deptos[[i]][[j]][c(1:8),5])==0,0,sum(Deptos[[i]][[j]][c(9:15),5]))
    Cost_otros[i,j]<-ifelse(sum(Deptos[[i]][[j]][c(1:8),5])==0,0,sum(Deptos[[i]][[j]][c(16:20),5]))
    Cost_fijos[i,j]<-ifelse(sum(Deptos[[i]][[j]][c(1:8),5])==0,0,sum(Deptos[[i]][[j]][c(21:22),5]))
    Cost_variables[i,j]<-ifelse(sum(Deptos[[i]][[j]][c(1:8),5])==0,0,sum(Deptos[[i]][[j]][c(1:20),5]))
}}

costos<-list(Cost_total,Cost_MO,Cost_insum,Cost_otros,Cost_fijos,Cost_variables)
for (i in 1:length(costos)){
  costos[[i]][costos[[i]] == 0] <-NA
}
names(costos)<-c("Costos Totales","Costos Mano de Obra","Costos Insumos","Costos Otros", "Costos Fijos","Costos Variables")
for (i in 1:length(costos)){
costos[[i]]<-setDT(costos[[i]], keep.rownames = TRUE)[];names(costos[[i]])[1]<-"depto"
costos[[i]]<-rbind(costos[[i]],costos[[i]][which(costos[[i]][,1]=="Cundinamarca"),]);costos[[i]][33,1]<-"Bogota"
costos[[i]]<-join(costos[[i]],codesDepto)
}

#1. https://www.dane.gov.co/index.php/estadisticas-por-tema/agropecuario/sistema-de-informacion-de-precios-sipsa/componente-insumos-1/componente-insumos-historicos
