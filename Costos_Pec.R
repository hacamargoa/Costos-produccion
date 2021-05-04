source('C:/Users/hac809/Desktop/FAO/Costos-produccion/Tablas_pecuario.R', encoding = 'UTF-8')

Tabla_costos_pec<-read.csv("../costos database/Tabla_costos_pec.csv", h=T)
DeptosV<-list()
for(j in Departamentos){
  Line<-list()
  for (i in 1:length(Lineas)){
    temp<-Tabla_costos_pec
    for (f in 1:length(UnitsV)){
      temp$Cantidad[f]<-as.numeric(UnitsV[[f]][UnitsV[[f]]$Departamento==j,Lineas[i]])
      temp$Valor_Uni[f]<-ValV[[f]][as.character(ValV[[f]]$Departamento)==j,Lineas[i]]
  }
    temp[c(15:17,19:21),3]<-1
    temp[15,4]<-60000
    temp[16,4]<-100000
    temp[17,4]<-200000
    temp[18,3]<-1500;temp[18,4]<-200
    temp[19,4]<-2000000/100
    temp[20,4]<-Arriendos[Arriendos$Departamento==j,2]/10
    temp$Costo<-as.numeric(temp$Cantidad)*as.numeric(temp$Valor_Uni)
    temp[21,c(4,5)]<-sum(temp[,5],na.rm = T)*0.06
    Line[[i]]<-temp
    
  }
  DeptosV[[j]]<-Line;names(DeptosV[[j]])<-Lineas
}





