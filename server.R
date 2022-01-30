library(ggplot2)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  almacen <- reactiveValues()
  
  output$hist <- renderPlot({
    input$add
    
    distribucion <- isolate(input$dist)
    par(mfrow=c(3,1))
    
    if (distribucion == "normal"){
      
      mean <- isolate(input$mean)
      nombre <- isolate(input$name)
      sd <- isolate(input$sd)
      req(mean, sd)
      
      
      almacen[[nombre]] <- rnorm(100000, mean, sd)

    }
    
    for (j in isolate(names(almacen))) {
      hist(almacen[[j]],
           col="orange",
           main=paste("Distribución", j),
           freq=FALSE)
    }
    
    })
  
  
  
  output$params <- renderUI({
    
    if (input$dist == "normal")
      # TagList crea un hermano dentro de un renderUI
      tagList(
        numericInput("mean",
                     label = "Media",
                     value = 0),
        numericInput("sd",
                     label = "Desviación típica",
                     value = 1))
    
    else if (input$dist == "log_normal")
      tagList(
        numericInput("mean",
                     label = "Media (log)",
                     value = 0),
        numericInput("max",
                     label = "Desviación típica (log)",
                     value = 1))
    
    else if (input$dist == "binomial")
      tagList(
        numericInput("size",
                     label = "Intentos",
                     value = 10),
      numericInput("prob",
                     label = "Probabilidad del suceso",
                     value = 5))
    
    else if (input$dist == "gamma")
      tagList(
        numericInput("shape",
                           label = "Shape / Forma",
                           value = 1),
              numericInput("rate",
                           label = "Scale / Escala",
                           value = 1))
    
    else if (input$dist == "chisq")
      tagList(
        numericInput("degrees",
                     label = "Grados de libertad",
                     value = 1),
        numericInput("param2",
                     label = "rellenar1",
                     value = 1),
        numericInput("param3",
                     label = "rellenar2",
                     value = 1))
    
    else if (input$dist == "truncated_normal")
      tagList(
        numericInput("minval",
                     label = "Valor mínimo",
                     value = -Inf),
        numericInput("maxval",
                     label = "Valor máximo",
                     value = Inf),
        numericInput("mean",
                     label = "Media",
                     value = 0),
        numericInput("sd",
                     label = "Desviación típica",
                     value = 1))
    
    
  })
  
  
}
