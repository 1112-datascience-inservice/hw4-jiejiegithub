library(shiny)
library(ggbiplot)

# Define UI
ui <- fluidPage(
  titlePanel("PCA Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xpc", "Choose X Principal Component:",
                  choices = c("PC1", "PC2", "PC3", "PC4"),
                  selected = "PC1"),
      selectInput("ypc", "Choose Y Principal Component:",
                  choices = c("PC1", "PC2", "PC3", "PC4"),
                  selected = "PC2")
    ),
    mainPanel(
      plotOutput("pca_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Load data
  data(iris)
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]
  
  # Apply PCA
  ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
  
  # Create plot
  output$pca_plot <- renderPlot({
    x_index <- match(input$xpc, c("PC1", "PC2", "PC3", "PC4"))
    y_index <- match(input$ypc, c("PC1", "PC2", "PC3", "PC4"))
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species,
                  choices = c(x_index, y_index), varname.size = 4)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)