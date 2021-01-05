library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Slovenske Regije"),
  
  tabsetPanel(
      tabPanel("Velikost proizvodnje",
               DT::dataTableOutput("druzine")),
      
      tabPanel("Število naselij",
               sidebarPanel(
                  uiOutput("pokrajine")
                ),
               mainPanel(plotOutput("naselja")))
    )
))
