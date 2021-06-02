source('C:/Users/hac809/Desktop/FAO/Costos-produccion/Tablas_pecuario.R', encoding = 'UTF-8')
#compilación de tablas de costos por departamento y linea productiva en la lista DeptosV
transportes<-c(40000,300000,20000,200,200,20000,20000)
Tabla_costos_pec<-read.csv("Tabla_costos_pec.csv", h=T)
DeptosV<-list()
for(j in Departamentos){
  Line<-list()
  for (i in 1:length(Lineas)){
    temp<-Tabla_costos_pec
    for (f in 1:length(UnitsV)){
      temp$Cantidad[f]<-as.numeric(UnitsV[[f]][UnitsV[[f]]$Departamento==j,Lineas[i]])
      temp$Valor_Uni[f]<-ValV[[f]][as.character(ValV[[f]]$Departamento)==j,Lineas[i]]
  }
    temp[c(16:22),3]<-1
    temp[16,4]<-ifelse(i==1|i==7,60000,0)
    temp[17,4]<-ifelse(i==1|i==7,60000,0)
    temp[18,4]<-ifelse(i==1|i==6|i==7,0,ifelse(i==4|i==5,200,ifelse(i==3,20000,50000)))
    temp[19,4]<-transportes[i]
    temp[20,4]<-ifelse(i==1|i==2,2000000/100,0)
    temp[21,4]<-ifelse(i==1|i==2,Arriendos[Arriendos$Departamento==j,2]/10,0)
    temp$Costo<-as.numeric(temp$Cantidad)*as.numeric(temp$Valor_Uni)
    temp[22,c(4,5)]<-sum(temp[,5],na.rm = T)*0.03
    Line[[i]]<-temp
    
  }
  DeptosV[[j]]<-Line;names(DeptosV[[j]])<-Lineas
}

#Calculo de tablas de costos para la creacion de mapas
costP<-data.frame(matrix(NA,nrow=32,ncol=length(Lineas)))
codesDepto<-read.csv("codedepto.csv",h=T)
codesDepto$id_depto<-sprintf("%02d",codesDepto$id_depto)
colnames(costP)<-Lineas;rownames(costP)<-c(Departamentos)
Cost_totalP<-costP
Cost_MOP<-costP
Cost_insumP<-costP 
Cost_otrosP<-costP
Cost_fijosP<-costP
Cost_variablesP<-costP
for(i in Departamentos){
  
  for (j in Lineas){
    Cost_totalP[i,j]<-sum(DeptosV[[i]][[j]][,5])
    Cost_MOP[i,j]<-sum(DeptosV[[i]][[j]][c(2:4),5])
    Cost_insumP[i,j]<-sum(DeptosV[[i]][[j]][c(5:14),5])
    Cost_otrosP[i,j]<-sum(DeptosV[[i]][[j]][c(1,15:19),5])
    Cost_fijosP[i,j]<-sum(DeptosV[[i]][[j]][c(20:22),5])
    Cost_variablesP[i,j]<-sum(DeptosV[[i]][[j]][c(1:19),5])
  }}

costosP<-list(Cost_totalP,Cost_MOP,Cost_insumP,Cost_otrosP,Cost_fijosP,Cost_variablesP)
names(costosP)<-c("Costos Totales","Costos Mano de Obra","Costos Insumos","Costos Otros", "Costos Fijos","Costos Variables")
for (i in 1:length(costosP)){
  costosP[[i]]<-setDT(costosP[[i]], keep.rownames = TRUE)[];names(costosP[[i]])[1]<-"depto"
  costosP[[i]]<-rbind(costosP[[i]],costosP[[i]][which(costosP[[i]][,1]=="Cundinamarca"),]);costosP[[i]][33,1]<-"Bogota"
  costosP[[i]]<-join(costosP[[i]],codesDepto)
}





