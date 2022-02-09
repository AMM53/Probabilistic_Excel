library(ggplot2)
library(randomcoloR)
library(tidyverse)
library(truncnorm)
library(dplyr)
library(shiny)

ui <- fluidPage(
  titlePanel("Probabilistic Excel"),
  tabsetPanel(
    
    tabPanel("Add new variable",
             
             sidebarLayout(
               sidebarPanel(
                 textInput("name", "Name:",value = "Example_Normal"),
                 
                 selectInput("dist",
                             "Probability Distribution",
                             choices = list(
                               "Normal" = "normal",
                               "Truncated Normal" = "truncated_normal",
                               "Log-normal" = "log_normal",
                               "Binomial" = "binomial",
                               "Gamma" = "gamma",
                               "X²" = "chisq"),
                             selected = "normal"),
                 
                 uiOutput("params"),
                 actionButton("add", label = "Add")
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(style = "overflow-y:scroll; max-height: 2000px; position:relative;",
                         uiOutput("plots")
                         
               )
             )),
    
    tabPanel("Calculator",
             verticalLayout(
               wellPanel(
                 textInput("calc", "Enter a name",
                           value="Example"),
                 textInput("expr", "Enter an Expresion",
                           value="#Example_Normal + 5"),
                 p(
                   "Use the '#' symbol to denote variables created previously, e.g. #Var1 + #Var2 ",
                 ),
                 actionButton("addexpr", label = "Calculate")),
               uiOutput("calcplots")
             )
    ),
    
    tabPanel("Summaries",
             mainPanel(style = "overflow-y:scroll; max-height: 2000px; position:relative;",
                       uiOutput("summary")))
    
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  observeEvent(input$add, {
    showNotification(paste("Distribution", input$name, "created"), type="message")
  })
  
  almacen <- reactiveValues()
  almacen_calc <- reactiveValues()
  
  
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
                     min = 0,
                     max = 1,
                     value = 0.5))
    
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
        numericInput("ncp",
                     label = "Parámetro de centralidad",
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
      plotOutput(plotname, height = 500, width = 1000)
      
    })
    
    do.call(tagList, plot_output_list)
  })
  
  output$summary <- renderUI({
    
    summary_output_list <- lapply(0:(input$addexpr), function(i) {
      summaryname <- paste("summary", i, sep="")
      verbatimTextOutput(summaryname)
      
    })
    
    do.call(tagList, summary_output_list)
  })
  

  output$calcplots <- renderUI({
    
    calcplot_output_list <- lapply(0:(input$addexpr), function(i) {
      calcplotname <- paste("calcplot", i, sep="")
      plotOutput(calcplotname, height = 500, width = 1000)
      
    })
    
    do.call(tagList, calcplot_output_list)
  })
  
  
  
  
  max_plots <- 100
  
  for (i in 0:max_plots) {
    
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      summaryname <- paste("summary", my_i, sep="")
      calcname <- paste("calcplot", my_i, sep="")
      
      
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
          
        } else if (distribucion =="binomial"){
          
          size <- isolate(input$size)
          nombre <- isolate(input$name)
          probability <- isolate(input$prob)
          req(size, probability)
          
          almacen[[nombre]] <- rbinom(100000, size, probability)
          
        } else if (distribucion =="truncated_normal"){
          
          min <- isolate(input$minval)
          max <- isolate(input$maxval)
          mean <- isolate(input$mean)
          sd <- isolate(input$sd)
          nombre <- isolate(input$name)
          req(min, max, mean, sd)
          
          almacen[[nombre]] <- rtruncnorm(100000, min, max, mean, sd)
          
        } else if (distribucion =="gamma"){
          
          shape <- isolate(input$shape)
          rate <- isolate(input$rate)
          nombre <- isolate(input$name)
          req(shape, rate)
          
          almacen[[nombre]] <- rgamma(100000, shape, rate)
          
        } else if (distribucion =="chisq"){
          
          degrees <- isolate(input$degrees)
          ncp <- isolate(input$ncp)
          
          nombre <- isolate(input$name)
          req(degrees, ncp)
          
          almacen[[nombre]] <- rchisq(100000, degrees, ncp)
          
        
          
        }
        
        print(tibble(value = almacen[[nombre]]) %>% 
                ggplot(aes(value))+ geom_histogram(fill=randomColor())+
                ggtitle(paste('Distribución',nombre)))
        
      })
      
      output[[summaryname]] <- renderPrint({

        
        print(paste("Distribution", isolate(input$calc)))
        summary(almacen_calc[[isolate(input$calc)]])
        
      })
      
      output[[calcname]] <- renderPlot({
        
        
        nombre_calculo <- if_else(isolate(input$calc)=="", isolate(input$expr), isolate(input$calc))
        
        expresion <- isolate(input$expr)
        expresion <- str_replace_all(expresion, "#", "almacen$")
        
        almacen_calc[[nombre_calculo]] <- isolate(eval(parse(text=expresion)))
        
        isolate(print(tibble(value = almacen_calc[[nombre_calculo]]) %>% 
                ggplot(aes(value))+ geom_histogram(fill=randomColor())+
                ggtitle(paste('Distribución', nombre_calculo))))
        
      })

      
      
    })
  }
  
}

shinyApp(ui=ui, server=server)

