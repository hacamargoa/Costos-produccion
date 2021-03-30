library(knitr)
library(dplyr)
library(kable)
library(kableExtra)
Tabla_costos_ag<-read.csv("Tabla_costos.csv", h=T)
Arriendos<-read.csv("Arriendos.csv", h=T)
Maquina<-read.csv("Maquina.csv", h=T)
Perc_propag<-c(0.224,0,0.064,0.054,0.200,0,0.111,0.176,0.175,0,0.186,0.168,0,0.2,0,0.075,0.205,0,0.16,0.102,0)
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

Depart="Arauca"
Cult="Platano_est"
if(Cult %in% Cultivos){
Data<-Deptos[[Depart]][[Cult]]
Data<-as.data.frame(lapply(Data,function(x)if(is.numeric(x))as.integer(x) else x))
sumrow <- data.frame(matrix("",nrow=1,ncol=5));names(sumrow)=names(Data)
subMo=sumrow
subMo[5]<-sum(Data[c(1:8),5])
subMo[1]<-"MANO DE OBRA"
subIns=sumrow
subIns[5]<-sum(Data[c(9:15),5])
subIns[1]<-"INSUMOS"
subOtr=sumrow
subOtr[5]<-sum(Data[c(16:20),5])
subOtr[1]<-"OTROS"
subFij=sumrow
subFij[5]<-sum(Data[c(21,22),5])
subFij[1]<-"COSTOS FIJOS"
sumrow[5]<-ifelse(sum(Data[c(1:15),5])==0,0,sum(Data[,5]))
sumrow[1]<-"COSTOS TOTALES"
Data1<-rbind(Data[c(1:8),],subMo,Data[c(9:15),],subIns,Data[c(16:20),],subOtr,Data[c(21,22),],subFij,sumrow)
if (sumrow[5]==0){
  print(paste0(strsplit(Cult,"_")[[1]][1]," no es una linea productiva significativa en ",Depart))
}else{
kable(Data1, row.names = FALSE,caption = paste0("<b>Costos de Produccion para una Hectarea de ",strsplit(Cult,"_")[[1]][1],
                                                ifelse(is.na(strsplit(Cult,"_")[[1]][2])," ",ifelse(strsplit(Cult,"_")[[1]][2]=="est"," establecimiento ", " sostenimiento"))," en ",Depart,"<b>"), booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(27, bold = T,italic=T) %>% # format last row
  row_spec(9, bold = T) %>% # format last row
  row_spec(17, bold = T) %>% # format last row
  row_spec(23, bold = T) %>% # format last row
  row_spec(26, bold = T) %>% # format last row
  column_spec(1, bold = T)  %>% # format first column
  footnote(ifelse(is.na(units[which(units[1]==Depart),Cult]),"Valores estimados con base en promedios de otros departamentos",""))  
  
  #cat(.,file=paste0("Costos de ",strsplit(Cult,"_")[[1]][1],ifelse(is.na(strsplit(Cult,"_")[[1]][2])," ",
                                                                  # ifelse(strsplit(Cult,"_")[[1]][2]=="est"," establecimiento ", " sostenimiento"))," en ",Depart,".html"))
}
}else{
  print(paste0(Cult, " no es una linea productiva incluida en esta herramienta"))
}

