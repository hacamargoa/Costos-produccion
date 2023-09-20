
cu=0
for (i in DeptosV){
  cu=cu+1
  cuen=0
  dir.create(paste0("C:/Users/hac809/Desktop/FAO/Entregables/Costos_estimados/Pecuarios1/",names(DeptosV)[cu],"/"))
  for (j in i){
    cuen=cuen+1
    write.csv(j,paste0("C:/Users/hac809/Desktop/FAO/Entregables/Costos_estimados/Pecuarios1/",names(DeptosV)[cu],"/",names(i)[cuen],".csv"),row.names = FALSE)
  }
}




