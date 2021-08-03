source('inputs_pec.R', encoding = 'UTF-8')
#Ingreso de las unidades por item y calculo de los valores
UnitsV<-import_list("tabla_de_costos_pec.xlsx")

Lineas<-colnames(UnitsV[[1]][, -1])
unitsV<-UnitsV[[1]]


#Especies
ValorES<-list()
Bovinos_carne<-c("Bovino Brahman x Pardo hembra 13-18 meses","Bovino Cebú hembra 13-18 meses","Bovino Cebú hembra 19-24 meses","Bovino Cebú hembra 37-48 meses",
           "Bovino Cebú macho 13-18 meses",	"Bovino Cebú macho 19-24 meses", "Bovino Cebú macho 25-36 meses", "Bovino Criollo hembra 13-18 meses", "Bovino Criollo macho 13-18 meses",
           "Bovino Criollo macho 37-48 meses")
Bovinos_leche<-c("Bovino Gyr X Brahman Rojo y Blanco macho 0-12 meses","Bovino Gyr X Brahman Rojo y Blanco macho 13-18 meses","Bovino Gyrolando hembra 0-12 meses",
            "Bovino Gyrolando hembra 13-18 meses","Bovino Holstein hembra 13-18 meses","Bovino Holstein hembra 19-24 meses","Bovino Holstein hembra 25-36 meses",
            "Bovino Normando hembra 0-12 meses", "Bovino Normando hembra 19-24 meses")
Porcinos<-c("Cerdo Criollo hembra-macho 45 días","Cerdo Landrace hembra 50-60 kilogramos","Cerdo Landrace hembra-macho 45 días","Cerdo Mejorado hembra 50-60 kilogramos",
       "Cerdo Mejorado hembra-macho 50-60 kilogramos","Cerdo Pic hembra-macho 45 días","Cerdo Pic hembra-macho 50-60 kilogramos",
       "Cerdo Pietran hembra 50-60 kilogramos","Cerdo Pietran hembra-macho 45 días","Cerdo Pietran hembra-macho 50-60 kilogramos")
Avícola_Ponedora<-c("Gallina/Polla ponedora Babcock Brown hembra 1 día","Gallina/Polla ponedora Babcock Brown hembra 15 semanas", "Gallina/Polla ponedora Hy-Line Brown hembra 1 día",
         "Gallina/Polla ponedora Lohmann Brown hembra 1 día", "Gallina/Polla ponedora Lohmann Brown hembra 15 semana")
Avícola_engorde<-c("Pollo/Polla engorde Cobb hembra-macho 1 día","Pollo/Polla engorde Cobb hembra-macho 15 días","Pollo/Polla engorde Ross hembra-macho 1 día")
Piscícola<-c("Alevino Bocachico hembra-macho 2,5-3,5 centímetros","Alevino Cachama Blanca macho 3-4 centímetros","Alevino Cachama Híbrida macho 2,5-3,5 centímetros",
        "Alevino Mojarra o Tilapia Roja macho 3-4 centímetros")
Caprinos<-vector();Ovinos<-vector()

Esp<-list(Bovinos_carne,Bovinos_leche,Porcinos,Avícola_Ponedora,Avícola_engorde,Piscícola,Caprinos,Ovinos)

semov<-insumos[[3]][[2]]
suppressWarnings(semov$number<-1/readr::parse_number(semov$Presentación))
#Pesos de animales por edad (1)
semov$number=ifelse(is.na(semov$number),ifelse(semov$Presentación=="kilogramo",ifelse(semov$`Nombre de la especie productiva` %like%  "%Cerdo%",ifelse(semov$`Nombre de la especie productiva` %like% "%50-60%",50,20),
                                                                                      ifelse(semov$`Nombre de la especie productiva` %like% "%37-48%"|semov$`Nombre de la especie productiva` %like% "%25-36%",470,
                                                                                             ifelse(semov$`Nombre de la especie productiva` %like% "%19-24%",350,250))),1),semov$number)
                                                           
semov=na.omit(semov)
for (j in Lineas){
Especies<-semov[semov$`Nombre de la especie productiva`%in% eval(parse(text=paste(j))),c(2,5,10,8)];names(Especies)=c("Departamento","Especie","Presentacion","Precio")
Especies$Precio<-as.numeric(Especies$Presentacion)*as.numeric(Especies$Precio)
ValorEs<-Valor
for (i in 1:nrow(ValorEs)){
  Dept<-iconv(ValorEs$Departamento[i],from="UTF-8",to="ASCII//TRANSLIT")
  temp<-subset(Especies,Especies[1]==as.character(Dept))
  av<-mean(as.numeric(temp[,4]))
  ValorEs$Valor[i]<-av
}
ValorES[[j]]<-ValorEs
}
ValorES[[7]][,2]<-300000
ValorES[[8]][,2]<-200000
#Usar Valor *3 para Jornales

#Enmiendas, Herbicidas, Insecticidas
# ValorE para enmiendas
#Insecticidas
InsecV<-data_list_ag[[5]][,c(2,11,12,13,14,17)]
InsecV<-subset(InsecV,Presentacion2=="litro"|Presentacion2=="litros"|Presentacion2=="kilogramo")
ValorInV<-Valor[1]
for (j in 1:nrow(ValorInV)){
  Dept<-iconv(ValorInV$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
  temp<-subset(InsecV,InsecV[1]==as.character(Dept))
  av<-mean(as.numeric(temp[,3],na.rm = TRUE))
  ValorInV$Valor[j]<-av
}
#Herbicidas
HerbiV<-data_list_ag[[4]][,c(2,11,12,13,14,17)]
HerbiV<-subset(HerbiV,Presentacion2=="litro"|Presentacion2=="litros"|Presentacion2=="kilogramo")
ValorHeV<-Valor[1]
for (j in 1:nrow(ValorHeV)){
  Dept<-iconv(ValorHeV$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
  temp<-subset(HerbiV,HerbiV[1]==as.character(Dept))
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
Lineas2<- sapply(strsplit(Lineas, "_"), "[[",1);Lineas2[c(4,5,6)]=c("Aves", "Aves", "Peces")
for(i in 1:length(Lineas2)){
Hormo<-data_list_pec[[4]][tolower(data_list_pec[[4]]$Linea) %like% paste0("%",tolower(Lineas2[i]),"%"),][c(2,8,10,11,12,14)];names(Hormo)[c(1,2)]<-c("Departamentos","Precio")
ValorHor<-Valor[1]
for (j in 1:nrow(ValorHor)){
  if(nrow(Hormo)!=0){
  Dept<-iconv(ValorHor$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
  temp<-subset(Hormo,Hormo[1]==as.character(Dept))
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
  Dept<-iconv(ValorMed$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
  temp<-subset(Med,Med[1]==as.character(Dept))
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
      Dept<-iconv(ValorVac$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp<-subset(Vac,Vac[1]==as.character(Dept))
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
      Dept<-iconv(ValorVit$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp<-subset(Vit,Vit[1]==as.character(Dept))
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
      Dept<-iconv(ValorAlim$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp<-subset(Alim,Alim[1]==as.character(Dept))
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

#Fuentes
#1. http://www.fao.org/ag/aga/agap/frg/lrrd/lrrd12/4/plas124a.htm Pesos cerdos por edad