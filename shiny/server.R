library(shiny)

shinyServer(function(input, output) {
  output$druzine <- DT::renderDataTable({
    podatki_Slovenija %>% pivot_wider(names_from="leto", values_from="Kolicina") 
  })
  
  output$pokrajine <- renderUI(
    selectInput("pokrajina", label="Izberi pokrajino",
                choices=c("Vse", podatki_Regija2$Regija))
  )
  output$pokrajine2 <- renderUI(
    selectInput("pokrajina2", label="Izberi pokrajino",
                choices=c("Vse", podatki_Regija2$Regija))
  )
  
  output$Regija2 <- renderPlot({
    main <- "Količina ton/ha proizvedenih kmetiskih produktov"
    if (!is.null(input$pokrajina2) && input$pokrajina2 %in% podatki_Regija$Regija) {
      t <- podatki_Regija %>% filter(Regija == input$pokrajina2)
      main <- paste(main, "v regiji", input$pokrajina2)
    } else {
      t <- podatki_Regija 
    }
    ggplot(t, aes(x=pridelek, y=Kolicina)) + geom_bar(stat = "identity") + 
      ggtitle(main) + xlab("Pridelki") + ylab("Količina ton/ha") + theme(axis.text.x = element_text(
        color="#000000", size=8, angle=75,hjust=0.5,vjust=0.9))
    
 
  })
  
  output$Regija <- renderPlot({
    main <- "Pogostost števila naselij"
    if (!is.null(input$pokrajina) && input$pokrajina %in% podatki_Regija2$Regija) {
      t <- podatki_Regija2 %>% filter(Regija == input$pokrajina)
      main <- paste(main, "v regiji", input$pokrajina)
    } else {
      t <- podatki_Regija2 %>% filter(Regija == "Goriška")
    }
    ggplot(data = t , aes(x=leto, y=Kolicina, color=pridelek, group = pridelek)) + xlab("Leto") + ylab("Količina ton/ha") + geom_line(aes(frame=leto)) +scale_fill_gradient(low = '#25511C', high='#2BFF00', limits = c(0,30)) 
  })
})
