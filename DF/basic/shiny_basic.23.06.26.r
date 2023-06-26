## 2023 Daiichi project
## human: patient cells
## 2023.04.14
setwd('~/Desktop/DF/DFCI_Paweletz/')
library(Seurat)
library(cowplot)
library(dplyr)
library(ggplot2)
obj.srt = readRDS('2023_BMS_DGKi_23.06/rds/P30290.filtered.988cells.23.06.23.rds')
umap = obj.srt@reductions$umap@cell.embeddings
umap[1:3,]
obj.srt@meta.data[1:3,] %>% ncol()
umap.meta.gs = cbind(obj.srt@reductions$umap@cell.embeddings, obj.srt@meta.data, 
                  obj.srt@assays$RNA@data[VariableFeatures(obj.srt),] %>% data.frame(check.names = F) %>% t() %>% data.frame())
umap.meta[1:3,]
umap.meta %>% write.csv('~/Desktop/test.umap.meta.csv')
obj.srt@assays$RNA@data[VariableFeatures(obj.srt),] %>% data.frame(check.names = F) %>% t() %>% data.frame() %>% 
  write.csv('~/Desktop/test.gene.exp.csv')
umap.meta.gs[1:3,1:3]
umap.meta.gs %>% write.csv('~/Desktop/test.gene.exp.umap.meta.csv')
umap.meta.gs %>% ncol()
test=read.csv('~/Desktop/test.gene.exp.umap.meta.csv', row.names = 1)
test[1:3,1:3]

#########################################
##### 여기서부터가 앱 
library(shiny)
library(ggplot2)
library(dplyr)
options(shiny.maxRequestSize=30*1024^2) ## increase max upload size
file1 = read.csv('~/Desktop/test.gene.exp.umap.meta.csv', row.names = 1)
#Rshiny input
ui <- fluidPage(
  #Input ui
  textInput(inputId="GENE", label="Please put gene name", 
            value="CD8A", placeholder=""),
  verbatimTextOutput("txt"), #Output ui
  plotOutput('plot')
)

#Rshiny Server 설정
server <- function(input, output) {
  output$txt <- renderPrint({
    req(input$GENE) #유효값 확인
    input$GENE
  })
  output$plot <- renderPlot({
    file1 %>% ggplot(aes(x= UMAP_1, y= UMAP_2, color=file1[,input$GENE])) + 
      geom_point() + theme_classic() +
      scale_color_gradient(low = 'grey', high = 'red') +
      theme(legend.title=element_blank())
  }, height = 400, width = 500)
}

#실행 
shinyApp(ui = ui, server = server)

