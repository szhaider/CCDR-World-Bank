#Climate Dashboard Tables
#This file maintains the Code for Tables in the App

##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_tables, {
  showModal(modalDialog(
    title = "How to use these tables",
    p("These tables give district level estimates of selected CCDR indicators in each province"), 
    p("All the values are rounded to 2 decimal points"),
    size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
})

#Code

#Updating Spatial Level based on selected domain
observeEvent(input$table_domain,{
if(input$table_domain == "Development Outcomes"){
  choices_tab_pol1 = spatial_level[1]
  
  updateSelectInput(
    getDefaultReactiveDomain(),
    "table_polygon",
    choices = choices_tab_pol1
  )
}else{
  choices_tab_pol2 = spatial_level
  updateSelectInput(
    getDefaultReactiveDomain(),
    "table_polygon",
    choices = choices_tab_pol2
  )
}
})

#Updating Indicators based on selected domain
observeEvent(input$table_domain, {
  if(input$table_domain == "Development Outcomes"){
    choices_ind_tab1 = data %>% 
      filter(domain == "Development Outcomes") %>% 
      distinct(indicator) %>% 
      pull(indicator)
      
    updateSelectInput(
        getDefaultReactiveDomain(),
        "table_indicator",
        choices = choices_ind_tab1
      )
  }else{
    choices_ind_tab2 = data %>% 
      filter(domain == "Natural Hazards") %>% 
      distinct(indicator) %>% 
      pull(indicator)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_ind_tab2
      )
  }
})

#Updating again to make tesil indicators from district go away, and vice versa
observeEvent(input$table_polygon,{
  if(input$table_polygon == "District" & input$table_domain == "Natural Hazards"){
  choices_haz_tab1 = data %>% 
    filter(domain == "Natural Hazards") %>% 
    distinct(indicator) %>% 
    slice_head(n=-3) %>% 
    pull(indicator)
    updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_haz_tab1
    )
  }else if(input$table_polygon == "Tehsil" & input$table_domain == "Natural Hazards") {
    choices_haz_tab2 = data %>% 
      filter(domain == "Natural Hazards") %>% 
      distinct(indicator) %>% 
      slice_tail(n=-3) %>% 
      pull(indicator)
    updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_haz_tab2
      )
  }else{
    choices_dev_tab2 = data %>% 
      filter(domain == "Development Outcomes") %>% 
      distinct(indicator) %>% 
      pull(indicator)
    updateSelectInput(
      getDefaultReactiveDomain(),
      "table_indicator",
      choices = choices_dev_tab2
    )
  }
})


#Table Data
tables_climate <- reactive({
  if(input$table_polygon == "District"){
  data %>% 
    filter(province ==  input$table_province,
           polygon ==   input$table_polygon,
           indicator == input$table_indicator) %>% 
  select(province, district, domain, indicator, value, -polygon, -tehsil) %>% 
  janitor::clean_names(case = "title") 
  }else{
    data %>% 
      filter(province ==  input$table_province,
             polygon ==   input$table_polygon,
             indicator == input$table_indicator) %>% 
      select(province, district, tehsil, domain, indicator, value, -polygon,) %>% 
      janitor::clean_names(case = "title") 
  }
})




output$tables_main <- renderDataTable({
  DT::datatable( 
    tables_climate(), 
    extensions = "Buttons",
    options= list(pageLength=38,
                  lengthChange = FALSE,
                  dom = "Blfrtip",
                  buttons = c("copy", "csv", "excel", "pdf")))
}) 

#Download Tables
output$downloadtable <- downloadHandler(
  filename = function(){
    paste0("table_", glue("{ input$table_province }", "_", "{ input$table_indicator }"), ".csv")
  },
  content = function(file){
    write.csv(tables_climate(), file)
  }
  
)
#Source table
output$source_table <- renderText({
  
    paste0("Source : CCDR Pakistan")
})
