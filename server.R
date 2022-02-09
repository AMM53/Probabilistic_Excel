library(ggplot2)
library(randomcoloR)
library(tidyverse)
library(truncnorm)
library(dplyr)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  almacen <- reactiveValues()
  
 # output$hist <- renderPlot({
 #   input$add
 #   
 #   distribucion <- isolate(input$dist)
 #   #par(mfrow=c(4, 1))
 #   
 #   if (distribucion == "normal"){
 #     
 #     mean <- isolate(input$mean)
 #     nombre <- isolate(input$name)
 #     sd <- isolate(input$sd)
 #     req(mean, sd)
 #     
 #     
 #     almacen[[nombre]] <- rnorm(100000, mean, sd)
#
 #   }
 #   
 #   #FIXME
 #   for (j in isolate(names(almacen))) {
 #     hist(almacen[[j]],
 #          col=randomColor(),
 #          main=paste("Distribución", j),
 #          freq=FALSE,
 #          xlab = NA)
 #   }
 #   
 #   })
 # 
  
  
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
  
  
  output$plots <- renderUI({
    
    plot_output_list <- lapply(0:(input$add), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 400, width = 600)
    })
    
    do.call(tagList, plot_output_list)
  })
  
  max_plots <- 100
  
  for (i in 0:max_plots) {

    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        
      distribucion <- isolate(input$dist)
      
      if (distribucion == "normal"){
       
       mean <- isolate(input$mean)
       nombre <- isolate(input$name)
       sd <- isolate(input$sd)
       req(mean, sd)
       
       
       almacen[[nombre]] <- rnorm(100000, mean, sd)
       
      } else if (distribucion =="log_normal"){
        
        mean <- isolate(input$mean)
        nombre <- isolate(input$name)
        sd <- isolate(input$max)
        req(mean, sd)
        
        almacen[[nombre]] <- rlnorm(100000, mean, sd)
        
      }
      
      
      print(tibble(value = almacen[[nombre]]) %>% 
              ggplot(aes(value))+ geom_histogram(fill=randomColor())+
              ggtitle(paste('Distribución',nombre, "#", my_i)))
      
    # hist(almacen[[nombre]],
    #      col=randomColor(),
    #      main=paste("Distribución", nombre),
    #      freq=FALSE,
    #      xlab = NA)
        
        
      })
    })
  }
  
  
  
 # output$plots <- renderUI({
 #   
 #   nombre <- isolate(input$name)
 #   
 #   output$nombre <- renderPlot({
 #     
 #     input$add
 #     
 #     distribucion <- isolate(input$dist)
 #     
 #     if (distribucion == "normal"){
 #       
 #       mean <- isolate(input$mean)
 #       nombre <- isolate(input$name)
 #       sd <- isolate(input$sd)
 #       req(mean, sd)
 #       
 #       
 #       almacen[[nombre]] <- rnorm(100000, mean, sd)
 #       
 #     }
 #     
 #     hist(almacen[[nombre]],
 #          col=randomColor(),
 #          main=paste("Distribución", nombre),
 #          freq=FALSE,
 #          xlab = NA)
 #     
 #     
 #     
 #   })
 #   
 #   
 #   
 # })
  
}
