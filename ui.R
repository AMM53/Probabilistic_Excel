library(shiny)


ui <- fluidPage(
  titlePanel("Probabilistic Excel"),
  tabsetPanel(
  
    tabPanel("Add new variable",

    sidebarLayout(
        sidebarPanel(
            textInput("name", "Name:",value = "Ejemplo (Normal (0,1))"),
            
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
        textInput("name", "Enter an Expresion"),
        p(
          "Use the '$' symbol to denote variables created previously, e.g. $Var1 + $Var2 ",
        ),
        actionButton("add", label = "Calculate")),
        plotOutput("finalplot")
    )
  ))
)


































