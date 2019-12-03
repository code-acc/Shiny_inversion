library(shiny)
library(ComplexHeatmap)
library(circlize)
library(viridisLite)
library(viridis)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Effect of chromosomal inversion"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("pop", label = "Select population:",
                         choices = list("Kalix Baltic Spring" = 1,
                                        "NorthSea_Atlantic_Autumn" = 2),
                         selected = 1),
      selectInput("chr", label = "Select chromosome:",
                  choices = c(paste0("chr", 1:26, "")),
                  selected = "chr12"),
      textInput("start", label = "Start position:",
                value = ""),
      textInput("stop", label = "Stop position:", 
                value = ""),
      submitButton("Submit"),
      br(),
      uiOutput("totals")
    ),
    
    mainPanel(
      plotOutput("heatMap")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  # Read the data
  dat <- read.table("dAF_chr12.csv", as.is=T)
  # plot_dat <- dat[c(dat$POS >= 20000000 & dat$POS <= 20050000), ]
  
 # # subset data based on chromosome selection
 # target.dat <- reactive({
 #   a <- dat
 #   if(input$chr != "chr12") a <- dat[dat$CHROM == input$chr, ]
 #   return(a)
 # })
 # 
 # subset data based on start and stop site selection
 target.dat <- reactive({
   # a <- dat[(dat$POS >= input$start & dat$POS <= input$stop), ]
   a <- subset(dat, dat$POS >= input$start & dat$POS <= input$stop)
   return(a)
 })

 # final data is now target.dat2()
  
 output$totals <- renderText({
   paste(nrow(target.dat()), "records available")
 })
 
 
 
  # Plotting heatmap of selected data
  output$heatMap <- renderPlot({
    
    a <- target.dat()
    
    col_fun = colorRamp2(c(0, 0.5, 1), c("white", "grey", "black"))
    col_fun(seq(-3, 3))
    ComplexHeatmap::Heatmap(as.matrix(t(a[, -(1:2)])), 
                                 cluster_columns = FALSE, col = col_fun)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
