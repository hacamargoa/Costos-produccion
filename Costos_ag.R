source('C:/Users/hac809/Desktop/FAO/Costos-produccion/Tablas_agricola.R', encoding = 'UTF-8')

library(knitr)
library(kableExtra)
#compilación de tablas de costos por departamento y linea productiva en la lista Deptos
Tabla_costos_ag<-read.csv("Tabla_costos.csv", h=T)
Arriendos<-read.csv("Arriendos.csv", h=T)
Maquina<-read.csv("Maquina.csv", h=T)
Perc_propag<-c(0.224,0,0.064,0.054,0.200,0,0.111,0.176,0.175,0,0.186,0.168,0,0.2,0,0.075,0.205,0,0.16,0.102,0,0.31,0,0,0.2,0.20,0.14,0.09)
Deptos<-list()
for(j in Departamentos){
  Cultiv<-list()
  for (i in 1:length(Cultivos)){
    temp<-Tabla_costos_ag
    for (f in 1:15){
      temp$CANTIDAD[f]<-as.numeric(Units[[f]][Units[[f]]$Departamento==j,Cultivos[i]])
      temp$Valor_Uni[f]<-Val3[[f]][Val3[[f]]$Departamento==j,Cultivos[i]]
    }
    
    temp[18,3]<-sum(temp[9,3],temp[10,3])/50;temp[18,4]<-4000
    temp[22,3]<-1;temp[22,4]=Arriendos[Arriendos$Departamento==j,2]
    temp[16,3]<-6;temp[16,4]<-Maquina[Maquina$Departamento==j,2]
    temp[20,3]<-1;temp[20,c(4)]<-500000
    temp$Costo<-as.numeric(temp$CANTIDAD)*as.numeric(temp$Valor_Uni)
    temp[19,3]<-1;temp[19,c(4,5)]<-sum(temp[,5])*0.06;temp[21,3]<-1;temp[21,c(4,5)]<-sum(temp[,5])*0.06
    temp[17,3]<-1;temp[17,c(4,5)]<-sum(temp[,5])*Perc_propag[i]
    Cultiv[[i]]<-temp
    }
  Deptos[[j]]<-Cultiv;names(Deptos[[j]])<-Cultivos
}
#Calculo de tablas de costos para la creacion de mapas
cost<-data.frame(matrix(NA,nrow=32,ncol=length(Cultivos)))
codesDepto<-read.csv("codedepto.csv",h=T)
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
names(costos)<-c("Costos Totales","Costos Mano de Obra","Costos Insumos","Costos Otros", "Costos Fijos","Costos Variables")
for (i in 1:length(costos)){
costos[[i]]<-setDT(costos[[i]], keep.rownames = TRUE)[];names(costos[[i]])[1]<-"depto"
costos[[i]]<-rbind(costos[[i]],costos[[i]][which(costos[[i]][,1]=="Cundinamarca"),]);costos[[i]][33,1]<-"Bogota"
costos[[i]]<-join(costos[[i]],codesDepto)
}
