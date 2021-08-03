source('inputs_ag.R', encoding = 'UTF-8')
library(DescTools)

#Ingreso las unidades por item y calculo de los valores de acuerdo a SIPSA
Units <- import_list("tabla_de_costos.xlsx")
Cultivos <- colnames(Units[[1]][, -1])
Departamentos <-Units[[1]][,1]
units <- Units[[1]]
for (i in 1:length(Units)) {
  Units[[i]][, 2] <- as.numeric(Units[[i]][, 2])
}
Jornal <- insumos[[3]][[3]][, c(2, 5, 7)]
Jornal <-subset(Jornal, Jornal[2] == "Jornal agrícola, sin alimentación")
Valor <- units[c(1, 2)]
names(Valor)[2] <- "Valor"
Valor[2] <- NA
for (i in 1:nrow(Valor)) {
  Dept<-iconv(Valor$Departamento[i],from="UTF-8",to="ASCII//TRANSLIT")
  temp <- subset(Jornal, Jornal[1] == as.character(Dept))
  av <- mean(as.numeric(temp[, 3]))
  Valor$Valor[i] <- av
}

#Enmiendas, Fertilizantes, Foliares y Coadyuvantes
Enmiendas <-subset(data_list_ag[[2]],
    Producto == "CAL" |
      Producto == "ABONO" |
      Producto == "NUTRISUELO" | Producto == "ABONISSA"
  )[, c(2, 5, 11, 12)]
ValorE <- Valor
for (i in 1:nrow(ValorE)) {
  Dept<-iconv(ValorE$Departamento[i],from="UTF-8",to="ASCII//TRANSLIT")
  temp <-  subset(Enmiendas, Enmiendas[1] == as.character(Dept))
  av <- mean(as.numeric(temp[, 4]))
  ValorE$Valor[i] <- av
}

Ferti <-
  subset(
    data_list_ag[[2]],
    Producto != "CAL" &
      Producto != "ABONO" &
      Producto != "NUTRISUELO" &
      Producto != "ABONISSA" &
      Presentacion1 > 20 &
      (Presentacion2 == "kilogramo" |
         Presentacion2 == "kilogramos")
  )[, c(2, 5, 11, 12)]
ValorFe <- Valor
for (i in 1:nrow(ValorFe)) {
  Dept<-iconv(ValorFe$Departamento[i],from="UTF-8",to="ASCII//TRANSLIT")
  temp <- subset(Ferti, Ferti[1] == as.character(Dept))
  av <- mean(as.numeric(temp[, 4]))
  ValorFe$Valor[i] <- av
}

Foliares <-
  subset(data_list_ag[[2]], Presentacion2 == "litro")[, c(2, 5, 11, 12)]
ValorF <- Valor
for (i in 1:nrow(ValorF)) {
  Dept<-iconv(ValorF$Departamento[i],from="UTF-8",to="ASCII//TRANSLIT")
  temp <-subset(Foliares, Foliares[1] == as.character(Dept))
  av <- mean(as.numeric(temp[, 4]))
  ValorF$Valor[i] <- av
}

Coadyu <-
  subset(data_list_ag[[1]], Presentacion2 == "litro")[, c(2, 5, 11, 12)]
ValorC <- Valor
for (i in 1:nrow(ValorC)) {
  Dept<-iconv(ValorC$Departamento[i],from="UTF-8",to="ASCII//TRANSLIT")
  temp <- subset(Coadyu, Coadyu[1] == as.character(Dept))
  av <- mean(as.numeric(temp[, 4]))
  ValorC$Valor[i] <- av
}
Val <- list(Valor, ValorE, ValorFe, ValorF, ValorC)

#Fungicidas, Herbicidas, Insecticidas incluyen solo productos reportados ante el ICA para la líinea productiva
#El costo se estimao como promedio poderado segun las ventas de ingredientes activos en Colombia
#Fungicidas
ValorFU <- list()
Cult2 <- sapply(strsplit(Cultivos, "_"), "[[", 1)
Cult2 <- iconv(Cult2,from="UTF-8",to="ASCII//TRANSLIT")
for (i in 1:length(Cult2)) {
  Fungi <-
    data_list_ag[[3]][data_list_ag[[3]]$CULTIVO %like% paste0("%", Cult2[i], "%"), ][, c(2, 11, 12, 13, 14, 17)]
  Fungi <-
    subset(
      Fungi,
      Presentacion2 == "litro" |
        Presentacion2 == "litros" | Presentacion2 == "kilogramo"
    )
  ValorFu <- Valor[1]
  if (nrow(Fungi) > 0) {
    for (j in 1:nrow(ValorFu)) {
      Dept<-iconv(ValorFu$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp <- subset(Fungi, Fungi[1] == as.character(Dept))
      total <- sum(temp$Perc, na.rm = TRUE)
      temp$Perc2 <- temp$Perc / total
      av <-sum(as.numeric(temp[, 3]) * temp$Perc2, na.rm = TRUE)
      if (av == 0) {
        temp <- subset(Fungi, Fungi[1] == as.character(Dept))
        av <- mean(as.numeric(temp[, 3]), na.rm = TRUE)
        av <- ifelse(av > 100000, 100000, av)
      }
      ValorFu$Valor[j] <- av
    }
  } else {
    Fungi <-
      subset(
        data_list_ag[[3]],
        Presentacion2 == "litro" |
          Presentacion2 == "litros" |
          Presentacion2 == "kilogramo"
      )[, c(2, 11, 12, 13, 14, 17)]
    for (j in 1:nrow(ValorFu)) {
      Dept<-iconv(ValorFu$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp <- subset(Fungi, Fungi[1] == as.character(Dept))
      av <- mean(as.numeric(temp[, 3]), na.rm = TRUE)
      av <- ifelse(av > 100000, 100000, av)
      ValorFu$Valor[j] <- av
    }
  }
  ValorFU[[i]] <- ValorFu
  names(ValorFU)[i] <- Cultivos[i]
}
ValorFu <-
  join_all(ValorFU, by = "Departamento")
colnames(ValorFu) <- c("Departamento", Cultivos)
#Herbicidas
ValorHE <- list()
for (i in 1:length(Cult2)) {
  Herbi <-
    data_list_ag[[4]][data_list_ag[[4]]$CULTIVO %like% paste0("%", Cult2[i], "%"), ][, c(2, 11, 12, 13, 14, 17)]
  Herbi <-
    subset(
      Herbi,
      Presentacion2 == "litro" |
        Presentacion2 == "litros" | Presentacion2 == "kilogramo"
    )
  ValorHe <- Valor[1]
  if (nrow(Herbi) > 0) {
    for (j in 1:nrow(ValorHe)) {
      Dept<-iconv(ValorHe$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp <- subset(Herbi, Herbi[1] == as.character(Dept))
      total <- sum(temp$Perc, na.rm = TRUE)
      temp$Perc2 <- temp$Perc / total
      av <-
        sum(as.numeric(temp[, 3]) * temp$Perc2, na.rm = TRUE)
      if (av == 0) {
        temp <- subset(Herbi, Herbi[1] == as.character(Dept))
        av <- mean(as.numeric(temp[, 3]), na.rm = TRUE)
      }
      ValorHe$Valor[j] <- av
    }
  } else {
    Herbi <-
      subset(
        data_list_ag[[4]],
        Presentacion2 == "litro" |
          Presentacion2 == "litros" |
          Presentacion2 == "kilogramo"
      )[, c(2, 11, 12, 13, 14, 17)]
    for (j in 1:nrow(ValorHe)) {
      Dept<-iconv(ValorHe$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp <- subset(Herbi, Herbi[1] == as.character(Dept))
      av <- mean(as.numeric(temp[, 3]), na.rm = TRUE)
      ValorHe$Valor[j] <- av
    }
  }
  ValorHE[[i]] <- ValorHe
  names(ValorHE)[i] <- Cultivos[i]
}
ValorHe <-
  join_all(ValorHE, by = "Departamento")
colnames(ValorHe) <- c("Departamento", Cultivos)
#insecticidas
ValorIN <- list()
for (i in 1:length(Cult2)) {
  Insect <-
    data_list_ag[[5]][data_list_ag[[5]]$CULTIVO %like% paste0("%", Cult2[i], "%"), ][, c(2, 11, 12, 13, 14, 17)]
  Insect <-
    subset(
      Insect,
      Presentacion2 == "litro" |
        Presentacion2 == "litros" | Presentacion2 == "kilogramo"
    )
  ValorIn <- Valor[1]
  if (nrow(Insect) > 0) {
    for (j in 1:nrow(ValorIn)) {
      Dept<-iconv(ValorIn$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp <-
        subset(Insect, Insect[1] == as.character(Dept))
      total <- sum(temp$Perc, na.rm = TRUE)
      temp$Perc2 <- temp$Perc / total
      av <-
        sum(as.numeric(temp[, 3]) * temp$Perc2, na.rm = TRUE)
      if (av == 0) {
        temp <-
          subset(Insect,
                 Insect[1] == as.character(Dept))
        av <- mean(as.numeric(temp[, 3]), na.rm = TRUE)
      }
      ValorIn$Valor[j] <- av
    }
  } else {
    Insect <-
      subset(
        data_list_ag[[5]],
        Presentacion2 == "litro" |
          Presentacion2 == "litros" |
          Presentacion2 == "kilogramo"
      )[, c(2, 11, 12, 13, 14, 17)]
    for (j in 1:nrow(ValorIn)) {
      Dept<-iconv(ValorIn$Departamento[j],from="UTF-8",to="ASCII//TRANSLIT")
      temp <-
        subset(Insect, Insect[1] == as.character(Dept))
      av <- mean(as.numeric(temp[, 3]), na.rm = TRUE)
      ValorIn$Valor[j] <- av
    }
  }
  ValorIN[[i]] <-
    ValorIn
  names(ValorIN)[i] <- Cultivos[i]
}
ValorIn <-
  join_all(ValorIN, by = "Departamento")
colnames(ValorIn) <- c("Departamento", Cultivos)

#Creacion de una lista con toda la información de valores creada además estimación de los departamentos sin datos
#como promedio de los departamentos con información
for (i in 1:length(Val)) {
  Val[[i]] <- cbind(Val[[i]][1], rep(Val[[i]][2], times = length(Cultivos)))
  colnames(Val[[i]]) <- c("Departamento", Cultivos)
}
Val1 <- append(rep(list(Val[[1]]), 8), Val[c(2:5)])
Val2 <- list(ValorHe, ValorIn, ValorFu)
Val3 <- append(Val1, Val2)

for (i in 1:length(Val3)) {
  for (j in 2:ncol(Val3[[i]])) {
    Val3[[i]][, j] <-
      ifelse(is.na(Val3[[i]][, j]), mean(as.numeric(Val3[[i]][which(Val3[[i]][, j] !=
                                                                      0), j]), na.rm = TRUE), Val3[[i]][, j])
    Units[[i]][, j] <-
      ifelse(is.na(Units[[i]][, j]), ifelse(sum(Units[[i]][, j], na.rm = T) ==
                                              0, 0, mean(as.numeric(Units[[i]][which(Units[[i]][, j] != 0), j]), na.rm =
                                                           TRUE)), Units[[i]][, j])
  }
}

#Fuentes
#1. https://www.ica.gov.co/getattachment/Areas/Agricola/Servicios/Regulacion-y-Control-de-Plaguicidas-Quimicos/Estadisticas/ESTADISTICAS-PLAGUICIDAS-2019-1.pdf.aspx?lang=es-CO
#2. https://www.ica.gov.co/getdoc/d3612ebf-a5a6-4702-8d4b-8427c1cdaeb1/registros-nacionales-pqua-15-04-09.aspx
#3. https://www.dane.gov.co/index.php/estadisticas-por-tema/agropecuario/sistema-de-informacion-de-precios-sipsa/componente-insumos-1/componente-insumos-historicos
