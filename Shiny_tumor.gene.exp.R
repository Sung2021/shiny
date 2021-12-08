library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)
chronic <- read.csv('~/Desktop/shiny/NIK_chronic.csv', row.names = 1)
tumor <- read.csv('~/Desktop/shiny/NIK_tumor.csv', row.names = 1)
df.tmp <- tumor

ui <- fluidPage(
  
  # Application title
  titlePanel("Check the expression of the gene"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("thegene", 'gene',
                  choices=rownames(df.tmp),
                  selected  = "Tcf7"),
      selectInput('thedata','data',
                  choices = c('tumor'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    
    gene <- input$thegene
    df.tmp <- tumor
    gene.exp.func <- function(gene,input.data=df.tmp){
      gene <- as.character(gene)
      if(gene %in% rownames(input.data)){
        df <- input.data[gene,] %>% melt() %>% data.frame()
        df$condition <- substr(df$variable,1,5)
        df2 <- df %>% group_by(condition) %>% summarize(avg=mean(value), sd=sd(value))
        p <- ggplot(df2, aes(condition,avg)) + geom_col()+ geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd)) + 
          ylab(gene) + theme_classic()
        print(p)
      }
      else{
        message('no gene in this dataset')
      }
    }
    gene.exp.func(gene)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
