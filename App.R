library(shiny)
library(shinythemes)

preguntas<-fluidPage(theme=shinytheme("superhero"),
navbarPage(
  "Costos de Produccion",
  tabPanel("Costos",
           sidebarPanel(
             tags$h3("Seleccione:"),
             selectInput("txt1", "Departamento:", sort(Departamentos)),
             selectInput("txt2", "Linea:", sort(Cultivos)),
           ),
           mainPanel(
             h3("Costos de Producción"),
             tableOutput("costos")
           )
           )
))

tabla<- function (input, output){
  
    output$costos <- function(){
    if(input$txt2 %in% Cultivos){
    Data<-Deptos[[input$txt1]][[input$txt2]]
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
      print(paste0(strsplit(input$txt2,"_")[[1]][1]," no es una linea productiva significativa en ",input$txt1))
    }else{
      kable(Data1, row.names = FALSE,caption = paste0("<b>Costos de Produccion para una Hectarea de ",strsplit(input$txt2,"_")[[1]][1],
                                                      ifelse(is.na(strsplit(input$txt2,"_")[[1]][2])," ",ifelse(strsplit(input$txt2,"_")[[1]][2]=="est"," establecimiento ", " sostenimiento"))," en ",input$txt1,"<b>"), 
            booktabs = TRUE,font_size=100) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
        row_spec(27, bold = T,italic=T) %>% # format last row
        row_spec(9, bold = T) %>% # format last row
        row_spec(17, bold = T) %>% # format last row
        row_spec(23, bold = T) %>% # format last row
        row_spec(26, bold = T) %>% # format last row
        column_spec(1, bold = T)  %>% # format first column
        footnote(ifelse(is.na(units[which(units[1]==input$txt1),input$txt2]),"Valores estimados con base en promedios de otros departamentos",""))  
      
              }
  }else{
    print(paste0(input$txt2, " no es una linea productiva incluida en esta herramienta"))
  }
 
    }
}
shinyApp(ui=preguntas, server =tabla)
