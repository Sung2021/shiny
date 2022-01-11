library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("What's in a Name?"),
  # CODE BELOW: Add select input named "sex" to choose between "M" and "F"
  selectInput('dimension', 'Select projection', choices = c("tSNE")),
  # Add plot output to display top 10 most popular names
  selectInput('gene', 'Select gene', choices = rownames(gene.exp)),

  plotOutput('plot_dimension')
  
  )
server <- function(input, output){
    df <- read.csv(url('https://raw.githubusercontent.com/Sung2021/shiny/main/cell_hashing/all.dimension.csv'),
                   row.names = 1)
    gene.exp <- read.csv(url('https://raw.githubusercontent.com/Sung2021/shiny/main/cell_hashing/all.scaled.var.csv'),
                      row.names = 1)
    gene.exp <- data.frame(t(gene.exp))

    output$plot_dimension <- renderPlot({
      p <- ggplot(df, aes(tSNE_1, tSNE_2)) + 
        geom_point(aes(color=gene.exp[,input$gene]), size=0.5) +
        ggtitle(as.character(input$gene)) +
        scale_color_gradient(low = 'grey',
                             high = 'dark blue') +
        theme_classic()
      print(p)
    
  })
}

shinyApp(ui = ui, server = server)
