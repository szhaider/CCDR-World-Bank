#Bar Charts (District)
##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_bar, {
  showModal(modalDialog(
    title = "How to use these bar charts",
    p("These bar charts give district level aggregations of BOOST indicators over the available years in each province"), 
    p("Units for Original Budget, Final Budget and Actual Expenses are in LCU - Billion Rs."),
    p("Units for per capita estimates are in levels LCU - Rs."),
    p("All values rounded to 3 decimal points"),
    # p("'Province Facet' will take the comparison to province level, where horizontal axes are fixed for comparison"),
    p("User might see a friendly message if the data for selected specification isn't available yet."),
    size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
})

bar_chart_data <- function(){
data %>% 
 filter(
        polygon == "District", 
        province == input$province_bar,
        # domain == input$domain_bar,
        indicator == input$indicator_bar) %>% 
    arrange(desc(value))
}

#HighChart
output$bar_chart <- renderHighchart({

hc_bar <- highchart() %>% 
  hc_xAxis(categories = bar_chart_data()$district) %>% #title = list(text= input$description_bar)
  hc_chart(type='column',inverted=T) %>% 
  hc_add_series(name =  input$indicator_bar, bar_chart_data()$value) %>%
  # hc_yAxis(title = list(text = input$year_bar)) %>% 
  hc_plotOptions(series = list(hover= list(enabled= TRUE, linewidth = 10, color= "red"))) %>% 
  hc_labels(title = list(text= input$domain_bar)) %>% 
  hc_title(text=input$indicator_bar) %>% 
  hc_subtitle(text = "Source: World Bank BOOST Data") %>% 
  hc_exporting(enabled = T,
               filename= input$domain_bar, 
               buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadCSV", "downloadXLS", "downloadPDF", "downloadSVG"))))  # %>% 
#   export_hc("bar_chart.js")
# 
hc_bar


})




  

