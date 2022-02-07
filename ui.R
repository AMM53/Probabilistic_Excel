library(shiny)


ui <- fluidPage(
  titlePanel("Probabilistic Excel"),
  tabsetPanel(
  
    tabPanel("Añadir Distribución",

    sidebarLayout(
        sidebarPanel(
            textInput("name", "Name:"),
            
            selectInput("dist",
                        "Distribución:",
                        choices = list(
                            "Normal" = "normal",
                            "Truncated Normal" = "truncated_normal",
                            "Log-normal" = "log_normal",
                            "Binomial" = "binomial",
                            "Gamma" = "gamma",
                            "X²" = "chisq"),
                        selected = "normal"),
            
            uiOutput("params"),
            
            actionButton("add", label = "Añadir la distribución")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hist")
        )
    )),
    tabPanel("Calcular")
    )
)