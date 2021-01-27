library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Slovenske Regije"),
  
  tabsetPanel(
      tabPanel("Velikost proizvodnje",
               DT::dataTableOutput("druzine")),
      
      tabPanel("Pridelek v regijah skozi leta",
               sidebarPanel(
                  uiOutput("pokrajine")
                ),
               mainPanel(plotOutput("Regija"))),
      
      tabPanel("Pridelek v regijah v vseh letih",
               sidebarPanel(
                 uiOutput("pokrajine2")
               ),
               mainPanel(plotOutput("Regija2")))
    )
  
))
