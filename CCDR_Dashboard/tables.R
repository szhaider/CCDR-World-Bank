#Climate Dashboard Tables

tables_climate <- reactive({
  data %>% 
    filter(province ==  input$table_province,
           polygon ==   input$table_polygon,
           indicator == input$table_indicator)
})


output$tables_main <- renderDataTable({
  tables_climate()
}) 