#Bar Charts (District)
##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_bar, {
  showModal(modalDialog(
    title = "How to use these bar charts",
    p("These interactive bar charts give district and tehsil level estimates of CCDR-Pakistan indicators for the selected province"), 
    p("Tehsil level estimates are available only for natural hazards"),
    p("All indicators are rounded to 2 decimal points"),
    # p("All Natural Hazards Indicators are rounded to 4 decimal points"),
      size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
})

#Update Spatial Input Based on selected domain
observeEvent(input$domain_bar,{
 
  if(input$domain_bar == "Development Outcomes"){ 
  updateSelectInput(
    getDefaultReactiveDomain(),
    "polygon_bar",
    choices = unique(data$polygon)[1]
  )
  }else{
    updateSelectInput(
      getDefaultReactiveDomain(),
      "polygon_bar",
      choices = unique(data$polygon)
    )
  }
})
  
#Update Indicators based on spatial level
observeEvent(input$polygon_bar,{
  observeEvent(input$domain_bar,{
if(input$domain_bar == "Natural Hazards" & input$polygon_bar == "District"){
    bar_choices_haz1 =  (list(`River flooding` = list("Expected mortality from river floods (population count)", "Expected mortality from river floods (% of ADM population)", "Expected damage to built-up assets from river floods (hectares)", "Expected damage to built-up assets from river floods (% of ADM built-up area)", "Expected exposure of agricultural land to river floods (hectares)", "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
                              `Coastal flooding` = list("Expected mortality from coastal floods (population count)", "Expected damage to built-up assets from coastal floods (hectares)", "Expected damage to built-up assets from coastal floods (% of ADM built-up area)"),   #"Expected mortality from coastal floods (% of ADM population)",
                              `Landslides` = list("Population exposed to medium or high landslide hazard (population count)", "Population exposed to medium or high landslide hazard (% of ADM population)","Built-up assets exposed to medium or high landslide hazard (Hectares)",  "Built-up assets exposed to medium or high landslide hazard (% of ADM built-up area)"),
                              `Drought` = list("Frequency of agricultural drought stress affecting at least 30% of arable land during Season 1/Kharif (percentage of historical period 1984-2022)","Frequency of agricultural drought stress affecting at least 30% of arable land during Season 2/Rabi (percentage of historical period 1984-2022)" ),
                              `Heat stress` = list("Expected exposure to heat stress (population count)", "Expected exposure to heat stress (% of ADM population)"),
                              `Air pollution`= list("Expected increase of mortality from air pollution (population count)", "Expected increase of mortality from air pollution (% of ADM population)"),
                              `Demography` = list("District Population"),
                              `Agriculture & Built-up Area` = list("Built-up area extent (Ha)", "Agricultural land extent (Ha)"))) 
      # hazards_options[-c(11, 24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_bar",
      choices =bar_choices_haz1
    )
}else if(input$domain_bar == "Natural Hazards" & input$polygon_bar == "Tehsil"){
  bar_choices_haz2 = (list(`River flooding` = list("Expected mortality from river floods (population count)", "Expected mortality from river floods (% of ADM population)", "Expected damage to built-up assets from river floods (hectares)", "Expected damage to built-up assets from river floods (% of ADM built-up area)", "Expected exposure of agricultural land to river floods (hectares)", "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
                           `Coastal flooding` = list("Expected mortality from coastal floods (population count)", "Expected damage to built-up assets from coastal floods (hectares)", "Expected damage to built-up assets from coastal floods (% of ADM built-up area)"),   #"Expected mortality from coastal floods (% of ADM population)",
                           `Landslides` = list("Population exposed to medium or high landslide hazard (population count)", "Population exposed to medium or high landslide hazard (% of ADM population)","Built-up assets exposed to medium or high landslide hazard (Hectares)",  "Built-up assets exposed to medium or high landslide hazard (% of ADM built-up area)"),
                           `Drought` = list("Frequency of agricultural drought stress affecting at least 30% of arable land during Season 1/Kharif (percentage of historical period 1984-2022)","Frequency of agricultural drought stress affecting at least 30% of arable land during Season 2/Rabi (percentage of historical period 1984-2022)" ),
                           `Heat stress` = list("Expected exposure to heat stress (population count)", "Expected exposure to heat stress (% of ADM population)"),
                           `Air pollution`= list("Expected increase of mortality from air pollution (population count)", "Expected increase of mortality from air pollution (% of ADM population)"),
                           `Demography` = list("Tehsil Population"),
                           `Agriculture & Built-up Area` = list("Tehsil Built-up area extent (Ha)", "Tehsil Agricultural land extent (Ha)"))) 
    # hazards_options[-c(1,2,3, 11)]
  updateSelectInput(
    getDefaultReactiveDomain(),
    "indicator_bar",
    choices = bar_choices_haz2
  )
}else{ 
  updateSelectInput(
    getDefaultReactiveDomain(),
    "indicator_bar",
    choices = development_options
  )
}
  })
})

bar_chart_data <- function(){
data %>% 
 filter(
        polygon == input$polygon_bar, 
        province == input$province_bar,
        domain == input$domain_bar,
        indicator_1 == input$indicator_bar,
        !is.na(value)) %>% 
     arrange(desc(value))
}

##Bar chart
output$bar_chart <- renderPlot({
   if(input$polygon_bar == "District"){
    bar_chart_data() %>%
      mutate(district = fct_reorder(district , value)) %>%
      ggplot(aes(y=district, x=value)) +
      geom_col(fill="midnightblue")+
       labs(y="", x= input$indicator_bar)
   }else{
     bar_chart_data() %>%
       mutate(tehsil = fct_reorder(tehsil , value)) %>%
       ggplot(aes(y=tehsil, x=value))+
       geom_col(fill="midnightblue")+
       labs(y="", x= input$indicator_bar)
   }
})

####
# High charter graph replaced with ggplot2 (enable for highchart)

##HighChart
# output$bar_chart <- renderHighchart({
# 
# if(input$polygon_bar == "District"){
# hc_bar <- highchart() %>%
#   hc_xAxis(categories = bar_chart_data()$district) %>% #title = list(text= input$description_bar)
#   hc_chart(type='column',inverted=T) %>%
#   hc_add_series(name =  input$indicator_bar, bar_chart_data()$value) %>%
#   hc_plotOptions(series = list(hover= list(enabled= TRUE, linewidth = 10, color= "red"))) %>%
#   hc_labels(title = list(text= input$domain_bar)) %>%
#   hc_title(text=input$province_bar) %>%
#   hc_subtitle(text = "Source: CCDR Pakistan") %>%
#   hc_exporting(enabled = T,
#                filename= input$domain_bar,
#                buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadCSV", "downloadXLS", "downloadPDF", "downloadSVG"))))  # %>%
# }else{
#   hc_bar <- highchart() %>%
#     hc_xAxis(categories = bar_chart_data()$tehsil) %>% #title = list(text= input$description_bar)
#     hc_chart(type='column',inverted=T) %>%
#     hc_add_series(name =  input$indicator_bar, bar_chart_data()$value) %>%
#     hc_plotOptions(series = list(hover= list(enabled= TRUE, linewidth = 10, color= "red"))) %>%
#     hc_labels(title = list(text= input$domain_bar)) %>%
#     hc_title(text=input$province_bar) %>%
#     hc_subtitle(text = "Source: CCDR Pakistan") %>%
#     hc_exporting(enabled = T,
#                  filename= input$domain_bar,
#                  buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadCSV", "downloadXLS", "downloadPDF", "downloadSVG"))))  # %>%
# }
# 
# hc_bar
# 
# })


# output$labels_bar <- renderText({
#   unit_bar <- reactive({
#     bar_chart_data() %>% 
#     distinct(unit) %>% 
#     pull(unit)
#   })
#   paste0("Units: ", unit_bar())
# })


  

