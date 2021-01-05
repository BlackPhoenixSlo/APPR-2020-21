library(shiny)

shinyServer(function(input, output) {
  output$druzine <- DT::renderDataTable({
    podatki_Slovenija %>% pivot_wider(names_from="leto", values_from="Kolicina") 
  })
  
  output$pokrajine <- renderUI(
    selectInput("pokrajina", label="Izberi pokrajino",
                choices=c("Vse", levels(obcine$Regija)))
  )
  output$naselja <- renderPlot({
    main <- "Pogostost števila naselij"
    if (!is.null(input$pokrajina) && input$pokrajina %in% levels(obcine$Regija)) {
      t <- obcine %>% filter(Regija == input$pokrajina)
      main <- paste(main, "v regiji", input$pokrajina)
    } else {
      t <- obcine
    }
    ggplot(t, aes(x=naselja)) + geom_histogram() +
      ggtitle(main) + xlab("Število regi") + ylab("Število občin")
  })
})
