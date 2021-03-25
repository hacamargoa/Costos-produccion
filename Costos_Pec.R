library(knitr)
library(dplyr)
library(kableExtra)


Tabla_costos_pec<-read.csv("../costos database/Tabla_costos_pec.csv", h=T)
Arriendos<-read.csv("../costos database/Arriendos.csv", h=T)
#Maquina<-read.csv("../costos database/Maquina.csv", h=T)
DeptosV<-list()
for(j in Departamentos){
  Line<-list()
  for (i in 1:length(Lineas)){
    temp<-Tabla_costos_pec
    for (f in 1:12){
      temp$Cantidad[f]<-as.numeric(UnitsV[[f]][UnitsV[[f]]$Departamento==j,Lineas[i]])
      temp$Valor_Uni[f]<-ValV[[f]][as.character(ValV[[f]]$Departamento)==j,Lineas[i]]
  }
    temp[13,4]<-60000
    temp[14,4]<-100000
    temp[15,4]<-200000
    temp[16,3]<-1500;temp[16,4]<-200
    temp[17,4]<-2000000/100
    temp[18,4]<-Arriendos[Arriendos$Departamento==j,2]/10
    temp$Costo<-as.numeric(temp$Cantidad)*as.numeric(temp$Valor_Uni)
    temp[19,c(4,5)]<-sum(temp[,5],na.rm = T)*0.06
    Line[[i]]<-temp
    
  }
  DeptosV[[j]]<-Line;names(DeptosV[[j]])<-Lineas
}

Depart="Cundinamarca"
Linea="Carne_bov"

Data<-DeptosV[[Depart]][[Linea]]
Data<-as.data.frame(lapply(Data,function(x)if(is.numeric(x))as.integer(x) else x))
sumrow <- data.frame(matrix("",nrow=1,ncol=5));names(sumrow)=names(Data)
subMo=sumrow
subMo[5]<-sum(Data[c(2:4),5])
subMo[1]<-"MANO DE OBRA"
subIns=sumrow
subIns[5]<-sum(Data[c(5:11),5])
subIns[1]<-"INSUMOS"
subOtr=sumrow
subOtr[5]<-sum(Data[c(1,12:17),5])
subOtr[1]<-"OTROS"
subFij=sumrow
subFij[5]<-sum(Data[c(18,19),5])
subFij[1]<-"COSTOS FIJOS"
sumrow[5]<-sum(Data[,5])
sumrow[1]<-"COSTOS TOTALES"
Data1<-rbind(Data[c(1:4),],subMo,Data[c(5:11),],subIns,Data[c(12:17),],subOtr,Data[c(18,19),],subFij,sumrow)
kable(Data1, row.names = FALSE,caption = paste0("Costos de Produccion para una Hectarea de ",Cult," en ",Depart), booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(24, bold = T,italic=T) %>% # format last row
  row_spec(5, bold = T) %>% # format last row
  row_spec(13, bold = T) %>% # format last row
  row_spec(20, bold = T) %>% # format last row
  row_spec(23, bold = T) %>% # format last row
  column_spec(1, bold = T) # format first column



