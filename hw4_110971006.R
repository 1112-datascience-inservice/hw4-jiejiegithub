library(shiny)
library(ggbiplot)
library(vegan)
library(FactoMineR)
library(factoextra)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("NCCU_DS2023_hw4_110971006"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xpc", "Choose X Principal Component:",
                  choices = c("PC1", "PC2", "PC3", "PC4"),
                  selected = "PC1"),
      selectInput("ypc", "Choose Y Principal Component:",
                  choices = c("PC1", "PC2", "PC3", "PC4"),
                  selected = "PC2"),
      selectInput("zpc", "Choose Z Principal Component(PCA 3D Beta版使用):",
                  choices = c("PC1", "PC2", "PC3", "PC4"),
                  selected = "PC3"),
      numericInput("num_data", "Number of raw data inputs:",
                   value = nrow(iris), min = 1, max = nrow(iris), step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("PCA Plot", plotOutput("pca_plot")),
        tabPanel("PCA 3D Plot Beta(可用滑鼠拖拉圖示)", plotlyOutput("pca_3D_plot")),
        tabPanel("PCA Extended Results", verbatimTextOutput("extended_results")),
        tabPanel("CA Plot", plotOutput("ca_plot")),
        tabPanel("CA Summary", verbatimTextOutput("ca_summary")),
        tabPanel("Input Data (Log)", tableOutput("input_data")),
        tabPanel("Result Data", tableOutput("result_data")),
        tabPanel("Raw Data", tableOutput("raw_data"))
      )
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
  
  # Create 3D PCA plot using plotly
  output$pca_3D_plot <- renderPlotly({
    x_index <- match(input$xpc, c("PC1", "PC2", "PC3", "PC4"))
    y_index <- match(input$ypc, c("PC1", "PC2", "PC3", "PC4"))
    z_index <- match(input$zpc, c("PC1", "PC2", "PC3", "PC4"))
    
    subset_log.ir <- log.ir[1:input$num_data, ]
    subset_ir.species <- ir.species[1:input$num_data]
    
    subset_ir.pca <- prcomp(subset_log.ir, center = TRUE, scale. = TRUE)
    
    
    # Get PC scores
    pc_scores <- predict(subset_ir.pca)
    
    # Create 3D scatter plot
    plot_data <- data.frame(
      x = pc_scores[, x_index],
      y = pc_scores[, y_index],
      z = pc_scores[, z_index],
      species = subset_ir.species
    )
    
    # Create plotly scatter plot
    plot <- plot_ly(
      data = plot_data,
      x = ~x,
      y = ~y,
      z = ~z,
      color = ~species,
      colors = "Set1",
      marker = list(
        size = 3
      ),
      type = "scatter3d",
      mode = "markers"
    )
    
    # Add axis labels
    plot <- plot %>% layout(scene = list(
      xaxis = list(title = input$xpc),
      yaxis = list(title = input$ypc),
      zaxis = list(title = input$zpc)
    ))
  })
  
  # Create plot
  output$pca_plot <- renderPlot({
    x_index <- match(input$xpc, c("PC1", "PC2", "PC3", "PC4"))
    y_index <- match(input$ypc, c("PC1", "PC2", "PC3", "PC4"))
    
    # Subset data based on user input
    subset_log.ir <- log.ir[1:input$num_data, ]
    subset_ir.species <- ir.species[1:input$num_data]
    
    # Apply PCA on subsetted data
    subset_ir.pca <- prcomp(subset_log.ir, center = TRUE, scale. = TRUE)
    
    g <- ggbiplot(subset_ir.pca, obs.scale = 1, var.scale = 1, groups = subset_ir.species,
                  choices = c(x_index, y_index), varname.size = 4)
    
    g <- g + scale_color_manual(values = c(hcl(0, 100, 65), hcl(240, 100, 65), hcl(120, 100, 65)))
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    
    # Add ellipse to the plot
    g <- g + stat_ellipse(aes(fill=groups), geom="polygon", alpha=0.1) 
    g <- g + scale_fill_manual(values = c("red", "blue", "green"))
    print(g)
  })
  
  # Display result data
  output$result_data <- renderTable({
    as.data.frame(ir.pca$x)
  })
  
  # Display input data
  output$input_data <- renderTable({
    log.ir
  })
  
  # Display extended results
  output$extended_results <- renderPrint({
    cat("PCA Summary:\n")
    print(summary(ir.pca))
    
    cat("\n\nPCA Loadings:\n")
    print(ir.pca$rotation)
    
    cat("\n\nPCA Scores (first 6 rows for brevity):\n")
    print(head(ir.pca$x))
  })
  
  
  # Display CA plot
  output$ca_plot <- renderPlot({
    # Subset data based on user input
    subset_iris <- head(iris[,1:4], input$num_data)
    
    # Perform correspondence analysis
    ca <- CA(subset_iris)
    
    # Plot the result
    fviz_ca_biplot(ca)
  })
  
  # Display CA summary
  output$ca_summary <- renderPrint({
    # Subset data based on user input
    subset_iris <- head(iris[,1:4], input$num_data)
    
    # Perform correspondence analysis
    ca <- CA(subset_iris)
    
    # Print the summary
    summary(ca)
  })
  
  # Display raw data
  output$raw_data <- renderTable({
    log.ir <- head(iris, input$num_data)
    log.ir$Index <- 1:nrow(log.ir)
    log.ir <- log.ir[, c("Index", colnames(log.ir)[-ncol(log.ir)])]
    log.ir
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)