library(ggplot2)
library(shiny)
library(shinythemes)
library(colmaps)
library(data.table)
source('C:/Users/hac809/Desktop/FAO/Costos-produccion/Costos_Pec.R', encoding = 'UTF-8')

preguntas<-fluidPage(theme=shinytheme("superhero"),
navbarPage(
  "Costos de Produccion",
  tabPanel("Costos agricolas",
           sidebarPanel(
             tags$h3("Seleccione:"),
             selectInput("txt1", "Departamento:", sort(Departamentos)),
             selectInput("txt2", "Linea:", sort(Cultivos)),
             downloadButton(outputId = "downloaderA", label = "Download")
             ),
           mainPanel(
             h3("Costos de Producción Agrícola"),
             tableOutput("costosAg")
           )
           ),
  tabPanel("Costos pecuarios",
           sidebarPanel(
             tags$h3("Seleccione:"),
             selectInput("txt1pec", "Departamento:", sort(Departamentos)),
             selectInput("txt2pec", "Linea:", sort(Lineas)),
             downloadButton(outputId = "downloaderP", label = "Download")
           ),
           mainPanel(
             h3("Costos de Producción Pecuaria"),
             tableOutput("costosPec")
           )
    ),
  tabPanel("Mapas Agrícolas",
           sidebarPanel(
             tags$h3("Seleccione:"),
             selectInput("txt1map", "Cultivo:", sort(Cultivos)),
             selectInput("txt2map", "Item:", names(costos)),
             downloadButton(outputId = "downloaderM", label = "Download Map")
             
           ),
           mainPanel(
             h3("Costos de Producción Agrícola"),
             plotOutput("Map", width = "auto", height=800)
           )
  ),
  tabPanel("Mapas Pecuarios",
           sidebarPanel(
             tags$h3("Seleccione:"),
             selectInput("txt1pmap", "Producto:", sort(Lineas)),
             selectInput("txt2pmap", "Item:", names(costosP)),
             downloadButton(outputId = "downloaderMP", label = "Download Map")
             
           ),
           mainPanel(
             h3("Costos de Producción Pecuaria"),
             plotOutput("MapP", width = "auto", height=800)
           )
  )
))

tabla<- function (input, output){
  
  CosAg<- reactive({
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
  })
  
    output$costosAg <- function(){
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
  }
    }
    
     CosPec<- reactive({
       DataV<-DeptosV[[input$txt1pec]][[input$txt2pec]]
       DataV<-as.data.frame(lapply(DataV,function(x)if(is.numeric(x))as.integer(x) else x))
       sumrowV <- data.frame(matrix("",nrow=1,ncol=5));names(sumrowV)=names(DataV)
       subMoV=sumrowV
       subMoV[5]<-sum(DataV[c(2:4),5])
       subMoV[1]<-"MANO DE OBRA"
       subInsV=sumrowV
       subInsV[5]<-sum(DataV[c(5:14),5])
       subInsV[1]<-"INSUMOS"
       subOtrV=sumrowV
       subOtrV[5]<-sum(DataV[c(15:19),5])
       subOtrV[1]<-"OTROS"
       subFijV=sumrowV
       subFijV[5]<-sum(DataV[c(20,22),5])
       subFijV[1]<-"COSTOS FIJOS"
       sumrowV[5]<-sum(DataV[1,5],subMoV[1,5],subInsV[1,5],subOtrV[1,5],subFijV[1,5])
       sumrowV[1]<-"COSTOS TOTALES"
       Data1V<-rbind(DataV[c(1:4),],subMoV,DataV[c(5:14),],subInsV,DataV[c(15:19),],subOtrV,DataV[c(20,22),],subFijV,sumrowV)
                         })
     
    output$costosPec <- function(){
      DataV<-DeptosV[[input$txt1pec]][[input$txt2pec]]
      DataV<-as.data.frame(lapply(DataV,function(x)if(is.numeric(x))as.integer(x) else x))
      sumrowV <- data.frame(matrix("",nrow=1,ncol=5));names(sumrowV)=names(DataV)
      subMoV=sumrowV
      subMoV[5]<-sum(DataV[c(2:4),5])
      subMoV[1]<-"MANO DE OBRA"
      subInsV=sumrowV
      subInsV[5]<-sum(DataV[c(5:14),5])
      subInsV[1]<-"INSUMOS"
      subOtrV=sumrowV
      subOtrV[5]<-sum(DataV[c(15:19),5])
      subOtrV[1]<-"OTROS"
      subFijV=sumrowV
      subFijV[5]<-sum(DataV[c(20,22),5])
      subFijV[1]<-"COSTOS FIJOS"
      sumrowV[5]<-sum(DataV[1,5],subMoV[1,5],subInsV[1,5],subOtrV[1,5],subFijV[1,5])
      sumrowV[1]<-"COSTOS TOTALES"
      Data1V<-rbind(DataV[c(1:4),],subMoV,DataV[c(5:14),],subInsV,DataV[c(15:19),],subOtrV,DataV[c(20,22),],subFijV,sumrowV)
      kable(Data1V, row.names = FALSE,caption = paste0("Costos de Produccion para ",input$txt2pec," en ",input$txt1pec), booktabs = TRUE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
        row_spec(24, bold = T,italic=T) %>% # format last row
        row_spec(5, bold = T) %>% # format last row
        row_spec(13, bold = T) %>% # format last row
        row_spec(20, bold = T) %>% # format last row
        row_spec(23, bold = T) %>% # format last row
        column_spec(1, bold = T)%>% # format first column
        footnote(ifelse(is.na(unitsV[which(unitsV[1]==input$txt1pec),input$txt2pec]),"Valores estimados con base en promedios de otros departamentos",""))
          }
    
    CosMap<-function(){
        colmap(departamentos, data = costos[[input$txt2map]], data_id = "id_depto",var=input$txt1map)+
        scale_fill_continuous(low = "#bcbddc", high = "#3f007d", na.value = "wheat")+
        ggtitle(paste(input$txt2map, "de ", input$txt1map))+
        theme(plot.title = element_text(hjust = 0.5, size =25, face ="bold"))
    }
    
    output$Map <- renderPlot({
      CosMap()
    })
    
    CosMapP<-function(){
      colmap(departamentos, data = costosP[[input$txt2pmap]], data_id = "id_depto",var=input$txt1pmap)+
        scale_fill_continuous(low = "#bcbddc", high = "#3f007d", na.value = "wheat")+
        ggtitle(paste(input$txt2pmap, "de ", input$txt1pmap))+
        theme(plot.title = element_text(hjust = 0.5, size =25, face ="bold"))
    }
    
    output$MapP <- renderPlot({
      CosMapP()
    })
    
    output$downloaderA <- downloadHandler(
      filename =function(){paste("Costos_", input$txt2, "_en_", input$txt1, ".csv",sep="")},
      content =function(file){
        write.csv(CosAg(), file,row.names = FALSE)
      })
    
    output$downloaderP <- downloadHandler(
        filename =function(){paste("Costos_", input$txt2pec, "_en_", input$txt1pec, ".csv",sep="")},
        content =function(file){
          write.csv(CosPec(), file,row.names = FALSE)
        })
    output$downloaderM <- downloadHandler(
      filename =function(){paste("Mapa", input$txt2map, "de ", input$txt1map, ".png",sep="")},
      content =function(file){
        png(file)
        print(CosMap())
        dev.off()
      })
    output$downloaderMP <- downloadHandler(
      filename =function(){paste("Mapa", input$txt2pmap, "de ", input$txt1pmap, ".png",sep="")},
      content =function(file){
        png(file)
        print(CosMapP())
        dev.off()
      })
   
}
shinyApp(ui=preguntas, server =tabla)
