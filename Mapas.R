Cost_total<-data.frame(matrix(NA,nrow=32,ncol=6))
colnames(Cost_total)<-Cultivos;rownames(Cost_total)<-Departamentos
for(i in Departamentos){
  for (j in Cultivos){
    Cost_total[i,j]<-sum(Deptos[[i]][[j]][,5])
  }
}
