source('C:/Users/hac809/Desktop/FAO/Costos-produccion/inputs_pec.R', encoding = 'UTF-8')

#Ingreso de las unidades por item y calculo de los valores
UnitsV<-import_list("tabla_de_costos_pec.xlsx")
Lineas<-colnames(UnitsV[[1]][, -1])
unitsV<-UnitsV[[1]]


#Especies
ValorES<-list()
for (j in 1:length(Lineas)){
a<-as.character(if(j==1)unique(insumos[[3]][[2]]$`Nombre de la especie productiva`)[c(6:14)] else unique(insumos[[3]][[2]]$`Nombre de la especie productiva`)[c(15:24)])
Especies<-insumos[[3]][[2]][insumos[[3]][[2]]$`Nombre de la especie productiva`%in% a,c(2,5,6,8)];names(Especies)=c("Departamento","Especie","Presentacion","Precio")
Especies$Precio<-ifelse(Especies$Presentacion=="kilogramo",as.numeric(Especies$Precio)*250,as.numeric(Especies$Precio))
ValorEs<-Valor
for (i in 1:nrow(ValorEs)){
  temp<-subset(Especies,Especies[1]==as.character(ValorEs$Departamento[i]))
  av<-mean(as.numeric(temp[,4]))
  ValorEs$Valor[i]<-av
}
ValorES[[j]]<-ValorEs
}
ValorES[[3]][,2]<-140000
ValorES[[4]][,2]<-1500
ValorES[[5]][,2]<-40
ValorES[[6]][,2]<-100000
ValorES[[7]][,2]<-100000
#Usar Valor *3 para Jornales

#Enmiendas, Herbicidas, Insecticidas
# ValorE para enmiendas
#Insecticidas
InsecV<-data_list_ag[[5]][,c(2,11,12,13,14,17)]
InsecV<-subset(InsecV,Presentacion2=="litro"|Presentacion2=="litros"|Presentacion2=="kilogramo")
ValorInV<-Valor[1]
for (j in 1:nrow(ValorInV)){
  temp<-subset(InsecV,InsecV[1]==as.character(ValorInV$Departamento[j]))
  av<-mean(as.numeric(temp[,3],na.rm = TRUE))
  ValorInV$Valor[j]<-av
}
#Herbicidas
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
ValorHOR<-list()
Lineas2<- sapply(strsplit(Lineas, "_"), "[[",1);Lineas2[c(4,5)]=c("Aves", "Peces")
for(i in 1:length(Lineas2)){
Hormo<-data_list_pec[[4]][tolower(data_list_pec[[4]]$Linea) %like% paste0("%",tolower(Lineas2[i]),"%"),][c(2,8,10,11,12,14)];names(Hormo)[c(1,2)]<-c("Departamentos","Precio")
ValorHor<-Valor[1]
for (j in 1:nrow(ValorHor)){
  if(nrow(Hormo)!=0){
  temp<-subset(Hormo,Hormo[1]==as.character(ValorHor$Departamento[j]))
  av<-mean(as.numeric(temp[,5],na.rm = TRUE))
  ValorHor$Valor[j]<-av
  }else{
    ValorHor$Valor[j]<-0
  }
}
ValorHOR[[i]]<-ValorHor;names(ValorHOR)[i]<-Lineas[i]
}

#Medicamentos
ValorMED<-list()
for(i in 1:length(Lineas2)){
Med<-data_list_pec[[6]][data_list_pec[[6]]$Linea %like% paste0("%",Lineas2[i],"%"),][c(2,8,10,11,12,14)];names(Med)[c(1,2)]<-c("Departamentos","Precio")
ValorMed<-Valor[1]
for (j in 1:nrow(ValorMed)){
  if(nrow(Med)!=0){
  temp<-subset(Med,Med[1]==as.character(ValorMed$Departamento[j]))
  av<-mean(as.numeric(temp[,2],na.rm = TRUE))
  ValorMed$Valor[j]<-av
  }else{
    ValorMed$Valor[j]<-0
  }
}
ValorMED[[i]]<-ValorMed;names(ValorMED)[i]<-Lineas[i]
}

#Vacunas
ValorVAC<-list()
for(i in 1:length(Lineas2)){
  Vac<-data_list_pec[[2]][data_list_pec[[2]]$Linea %like% paste0("%",Lineas2[i],"%"),][c(2,8,10,11,12,14)];names(Vac)[c(1,2)]<-c("Departamentos","Precio")
  ValorVac<-Valor[1]
  for (j in 1:nrow(ValorVac)){
    if(nrow(Vac)!=0){
      temp<-subset(Vac,Vac[1]==as.character(ValorVac$Departamento[j]))
      av<-mean(as.numeric(temp[,2],na.rm = TRUE))
      ValorVac$Valor[j]<-av
    }else{
      ValorVac$Valor[j]<-0
    }
  }
  ValorVAC[[i]]<-ValorVac;names(ValorVAC)[i]<-Lineas[i]
}

#Vitaminas y suplementos
ValorVIT<-list()
for(i in 1:length(Lineas2)){
  Vit<-data_list_pec[[7]][data_list_pec[[7]]$Linea %like% paste0("%",Lineas2[i],"%"),][c(2,8,10,11,12,14)];names(Vit)[c(1,2)]<-c("Departamentos","Precio")
  ValorVit<-Valor[1]
  for (j in 1:nrow(ValorVit)){
    if(nrow(Vit)!=0){
      temp<-subset(Vit,Vit[1]==as.character(ValorVit$Departamento[j]))
      av<-mean(as.numeric(temp[,2],na.rm = TRUE))
      ValorVit$Valor[j]<-av
    }else{
      ValorVit$Valor[j]<-0
    }
  }
  ValorVIT[[i]]<-ValorVit;names(ValorVIT)[i]<-Lineas[i]
}

#Alimento
ValorALIM<-list()
data_list_pec[[1]]$Linea<-ifelse(data_list_pec[[1]]$Producto %in% c("CERDAS","CERDOS"),"Porcinos",
                                 ifelse(data_list_pec[[1]]$Producto %in% c("MOJARRA","TRUCHA"),"Peces",
                                        ifelse(data_list_pec[[1]]$Producto %in% c("POLLA","POLLITA","POLLITO","POLLO","PONEDORAS"),"Aves", "Bovinos")))
data_list_pec[[1]]$Precio<-data_list_pec[[1]]$prec.dosis*100
for(i in 1:length(Lineas2)){
  Alim<-data_list_pec[[1]][data_list_pec[[1]]$Linea %like% paste0("%",Lineas2[i],"%"),][c(2,15,10,11,12,14)];names(Alim)[1]<-c("Departamentos")
  ValorAlim<-Valor[1]
  for (j in 1:nrow(ValorAlim)){
    if(nrow(Alim)!=0){
      temp<-subset(Alim,Alim[1]==as.character(ValorAlim$Departamento[j]))
      av<-mean(as.numeric(temp[,2],na.rm = TRUE))
      ValorAlim$Valor[j]<-av
    }else{
      ValorAlim$Valor[j]<-0
    }
  }
  ValorALIM[[i]]<-ValorAlim;names(ValorALIM)[i]<-Lineas[i]
}

#Mecanizacion
ValorMec<-Valor[1]
ValorMec$Valor<-90000

#Compilación de bases de datos de valores y unidades en listas, estimación de departamentos sin información 

ValV1<-list(Valor,Valor, Valor, ValorE,ValorInV,ValorHeV,ValorSem,ValorSal,ValorMec)
for (i in 1:length(ValV1)){
  ValV1[[i]]<-cbind(ValV1[[i]][1], rep(ValV1[[i]][2], times = length(Lineas)))
  colnames(ValV1[[i]]) <- c("Departamento", Lineas)
}

ValV2<-list(ValorES,ValorHOR,ValorMED,ValorVAC,ValorVIT,ValorALIM)
for (i in 1:length(ValV2)){
ValV2[[i]]<-join_all(ValV2[[i]],by="Departamento");colnames(ValV2[[i]])[c(2:length(ValV2[[i]]))]<-Lineas
}
ValV<-append(ValV1,ValV2)
ValV<-list(ValV[[10]],ValV[[1]],ValV[[2]],ValV[[3]],ValV[[4]],ValV[[5]],ValV[[6]],ValV[[7]],ValV[[8]],ValV[[11]],ValV[[12]],ValV[[13]],ValV[[14]],ValV[[15]],ValV[[9]])
for (i in 1:length(ValV)){
  for (j in 2:ncol(ValV[[i]])){
ValV[[i]][,j]<-ifelse(is.na(ValV[[i]][,j]), mean((ValV[[i]][,j]),na.rm=TRUE),ValV[[i]][,j])
UnitsV[[i]][,j]<-ifelse(is.na(UnitsV[[i]][,j]), mean((UnitsV[[i]][,j]),na.rm=TRUE),UnitsV[[i]][,j])
  }}

