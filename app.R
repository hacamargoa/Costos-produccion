library(ggplot2)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(colmaps)
library(data.table)
library(gridExtra)
library(datasets)
source('Costos_Pec.R', encoding = 'UTF-8')


ui<-fluidPage(
  setBackgroundImage(
    src = "Bkg_2.jpg"
  ),
  theme=shinytheme("simplex"),
  titlePanel(
    fluidRow(
      column(5, "Costos de Producción",style='padding-top:25px;font-size:45px;'),
      column(2, img(height = 100, src = "ECHO.png"),style='padding-left:0px; padding-right:20px; padding-top:0px; padding-bottom:0px;'), 
      column(2, img(height = 70, src = "fao_azul1.png"),style='padding-left:10px; padding-right:20px; padding-top:10px; padding-bottom:0px;'),
      column(2, img(height = 100, src = "MINAGRICULTURA.png"))
    )
    #padding-left:10px; padding-right:0px; padding-top:30px; padding-bottom:0px
  ),
  navbarPage("", 
  tabPanel("Costos Agrícolas",
             sidebarPanel(
             tags$h3("Seleccione:"),
             selectInput("txt1", "Departamento:", sort(Departamentos)),
             selectInput("txt2", "Línea:", sort(Cultivos)),
             downloadButton(outputId = "downloaderA", label = "Descargar csv"),
             downloadButton(outputId = "downloaderA2", label = "Descargar pdf"),
             ),
           
           mainPanel(
             h3("Costos de Producción Agrícola"),
             tableOutput("costosAg")
           )
           ),
  tabPanel("Costos Pecuarios",
           sidebarPanel(
             tags$h3("Seleccione:"),
             selectInput("txt1pec", "Departamento:", sort(Departamentos)),
             selectInput("txt2pec", "Línea:", sort(Lineas)),
             downloadButton(outputId = "downloaderP", label = "Descargar csv"),
             downloadButton(outputId = "downloaderP2", label = "Descargar pdf")
             
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
             downloadButton(outputId = "downloaderM", label = "Descargar Mapa"),
             div(style="height:50px;;",HTML("<br()>"))
             
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
             downloadButton(outputId = "downloaderMP", label = "Descargar Mapa")
             
           ),
           mainPanel(
             h3("Costos de Producción Pecuaria"),
             plotOutput("MapP", width = "auto", height=800)
           )
  )
)
)

server<- function (input, output){
  
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
        row_spec(26, bold = T,italic=T) %>% # format last row
        row_spec(1, bold = T) %>% # format last row
        row_spec(5, bold = T) %>% # format last row
        row_spec(16, bold = T) %>% # format last row
        row_spec(22, bold = T) %>% # format last row
        row_spec(25, bold = T) %>% # format last row
        column_spec(1, bold = T)%>% # format first column
        footnote(ifelse(is.na(unitsV[which(unitsV[1]==input$txt1pec),input$txt2pec]),"Valores estimados con base en promedios de otros departamentos",""))
          }
    
    CosMap<-function(){
        colmap(departamentos, data = costos[[input$txt2map]], data_id = "id_depto",var=input$txt1map)+
        scale_fill_distiller(palette ="YlOrRd", trans = "reverse", na.value = "snow3",name = "Miles de Pesos/ha", 
                              labels = scales::unit_format(unit = "",scale = 1e-3),guide=guide_legend(reverse = TRUE)) +
        ggtitle(paste(input$txt2map, "de ", input$txt1map))+ theme(plot.title = element_text(hjust = 0.5, size =25, face ="bold"),
                                                                   legend.title = element_text(hjust = 0.5, size =20, face ="bold"), 
              legend.background = element_rect(fill = "transparent", color = "black"),legend.text = element_text(hjust = 0.5, size =15),
              legend.margin = margin(0.5, 1, 1, 1, "cm"),legend.key.size = unit(2, "lines"))
        
    }
    
    output$Map <- renderPlot({
      CosMap()
    })
    
    CosMapP<-function(){
      colmap(departamentos, data = costosP[[input$txt2pmap]], data_id = "id_depto",var=input$txt1pmap)+
        scale_fill_distiller(palette ="YlOrRd", trans = "reverse", na.value = "snow3",
                             name = paste0("Miles de Pesos/", ifelse(input$txt1pmap=="Piscícola","Kg",ifelse(input$txt1pmap=="Avícola_Ponedora"|input$txt1pmap=="Bovinos_leche"|input$txt1pmap=="Ovinos","animal/año", "animal/ciclo"))),
                              labels = scales::unit_format(unit = "",scale = 1e-3),guide=guide_legend(reverse = TRUE)) +
        ggtitle(paste(input$txt2pmap, "de ", input$txt1pmap))+ theme(plot.title = element_text(hjust = 0.5, size =25, face ="bold"),
                                                                     legend.title = element_text(hjust = 0.5, size =20, face ="bold"), 
              legend.background = element_rect(fill = "white", color = "black"),legend.text = element_text(hjust = 0.5, size =15),
              legend.margin = margin(0.5, 1, 1, 1, "cm"),legend.key.size = unit(2, "lines"))
      
    }
    
    output$MapP <- renderPlot({
      CosMapP()
    })
    
    output$downloaderA <- downloadHandler(
      filename =function(){paste("Costos_", input$txt2, "_en_", input$txt1, ".csv",sep="")},
      content =function(file){
        write.csv(CosAg(), file,row.names = FALSE)
      })
    output$downloaderA2 <- downloadHandler(
      filename =function(){paste("Costos_", input$txt2, "_en_", input$txt1, ".pdf",sep="")},
      content =function(file){
        pdf(file,onefile=TRUE,height=11,width=9)
        grid.table(CosAg(),row=NULL)
        dev.off()
      })
    
    output$downloaderP <- downloadHandler(
        filename =function(){paste("Costos_", input$txt2pec, "_en_", input$txt1pec, ".csv",sep="")},
        content =function(file){
          write.csv(CosPec(), file,row.names = FALSE)
        })
    
    output$downloaderP2 <- downloadHandler(
      filename =function(){paste("Costos_", input$txt2pec, "_en_", input$txt1pec, ".pdf",sep="")},
      content =function(file){
        pdf(file,onefile=TRUE,height=11,width=9)
        grid.table(CosPec(),row=NULL)
        dev.off()
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
shinyApp(ui=ui, server =server)
