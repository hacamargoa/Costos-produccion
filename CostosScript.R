install.packages("rio")
install.packages("fuzzyjoin")
library(rio)
library(plyr)
library(dplyr)
library(fuzzyjoin)
library(stringer)
RegistroICA<-read.csv("../costos database/Registro_ICA2020.csv", h=T)
RegistroICA<-RegistroICA[,-c(1:3,5,7:11,13)];names(RegistroICA)[1]<-"Producto"
RegistroICA<-subset(RegistroICA,Producto!= "")
RegistroICA$Producto<-gsub( "®", " ", as.character(RegistroICA$Producto), 1)
RegistroICA$Producto<-sapply(strsplit(as.character(RegistroICA$Producto), " "), "[[",1)

RegistroICA$Producto
data_list <- import_list("C:/Users/hac809/Desktop/FAO/costos database/Anexos_Insumos_dic_2020.xlsx")
index<-na.omit(data_list[[1]][,-1]);colnames(index)<-c("Id","Cont")
data_list<-data_list[c(3:18)]
data_list_ag<-data_list[grepl("1.",names(data_list))]
data_list_pec<-data_list[grepl("2.",names(data_list))]
data_list_other<-data_list[grepl("3.",names(data_list))]
rem_last_col<-function(x){
  x[,-ncol(x)]
   }
insumos<-list(data_list_ag,data_list_pec,data_list_other)
for (j in 1:length(insumos)){
insumos[[j]]<-lapply(insumos[[j]],FUN=rem_last_col)
insumos[[j]]<-lapply(insumos[[j]],FUN=na.omit)
data_list2<-list()
for (i in 1:length(insumos[[j]])){
  temp<-insumos[[j]][[i]]
  names(temp)=temp[1,]
  names(temp)[names(temp)=="Nombre del producto"] <-"Producto"
  temp<-temp[-1,]
  insumos[[j]][[i]]<-temp
}
}
##Insumos agricolas
data_list_ag<-insumos[[1]]
for (i in 1:length(data_list_ag)){
  data_list_ag[[i]]$Producto<-gsub( "®", " ", as.character(data_list_ag[[i]]$Producto), 1)
  data_list_ag[[i]]$Producto<-sapply(strsplit(as.character(data_list_ag[[i]]$Producto), " "), "[[",1)
  data_list_ag[[i]]$Producto<-toupper(data_list_ag[[i]]$Producto)
}
data_list_ag<-lapply(data_list_ag,join,RegistroICA,by="Producto")  

