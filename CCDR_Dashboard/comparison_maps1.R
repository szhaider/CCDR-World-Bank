#Comaprison leaflet proxy#Comparison Maps
##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_comp, {
  showModal(modalDialog(
    title = "How to use these maps",
    p("These maps offer comparison (spatial correlation) between selected CCDR-Pakistan indicators"),
    # p("MAP1 refers to"),
    p("These maps give district and tehsil level estimates of CCDR-Pakistan indicators over the selected filters"),
    p("Tehsil level maps are available only for Natural Hazards indicators"),
    p("All indicators are rounded to 2 decimal points"),
    # p("All Natural Hazards Indicators are rounded to 3 decimal points"),
    p("Expect the color mapping to reverse with the context of  the selected indicators - e.g. Poverty (High) = Red whereas; Access to improved toilet facilities (High) = Blue"),
    size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
})


#code
#Map1

#shape file used
pak_shp_comp <-  reactive({
  pak_shp[[input$polygon_map1]]
})

#Development outcomes Data MAP1
map_data1 <- reactive({
  # req(input$domain_map1 == "Natural Hazards" || input$domain_map1 == "Development Outcomes")
  data %>% 
    filter(polygon   %in%  input$polygon_map1,
           domain    %in%  input$domain_map1,
           indicator_1 %in%  input$indicator_map1)
})

#Updating Indicators based on Domain
observeEvent(input$domain_map1,{
  if( input$domain_map1 == "Development Outcomes"){
    choices_dev_c1 =  data %>% 
      filter(domain == "Development Outcomes") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = choices_dev_c1
    )
  }else if(input$domain_map1 == "Relative Wealth Index"){
    choices_dev_c1 =  data %>% 
      filter(domain == "Relative Wealth Index") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = choices_dev_c1
    )
  }else {
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = hazards_options[-11])
  }
})

observeEvent(input$domain_map1,{
observeEvent(input$polygon_map1,{
  if(input$polygon_map1 == "District" & input$domain_map1 == "Natural Hazards"){
    dis_haz_choices_c1 = (list(`River flooding` = list("Expected mortality from river floods (population count)", "Expected mortality from river floods (% of ADM population)", "Expected damage to built-up assets from river floods (hectares)", "Expected damage to built-up assets from river floods (% of ADM built-up area)", "Expected exposure of agricultural land to river floods (hectares)", "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
                               `Coastal flooding` = list("Expected mortality from coastal floods (population count)", "Expected damage to built-up assets from coastal floods (hectares)", "Expected damage to built-up assets from coastal floods (% of ADM built-up area)"),   #"Expected mortality from coastal floods (% of ADM population)",
                               `Landslides` = list("Population exposed to medium or high landslide hazard (population count)", "Population exposed to medium or high landslide hazard (% of ADM population)","Built-up assets exposed to medium or high landslide hazard (Hectares)",  "Built-up assets exposed to medium or high landslide hazard (% of ADM built-up area)"),
                               `Drought` = list("Frequency of agricultural drought stress affecting at least 30% of arable land during Season 1/Kharif (percentage of historical period 1984-2022)","Frequency of agricultural drought stress affecting at least 30% of arable land during Season 2/Rabi (percentage of historical period 1984-2022)" ),
                               `Heat stress` = list("Expected exposure to heat stress (population count)", "Expected exposure to heat stress (% of ADM population)"),
                               `Air pollution`= list("Expected increase of mortality from air pollution (population count)", "Expected increase of mortality from air pollution (% of ADM population)"),
                               `Demography` = list("District Population"),
                               `Agriculture & Built-up Area` = list("Built-up area extent (Ha)", "Agricultural land extent (Ha)"))) 
      # hazards_options[-c(11,24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = dis_haz_choices_c1,
      selected =  dis_haz_choices_c1$`Agriculture & Built-up Area`[[2]]
    )
  }else if(input$polygon_map1 == "Tehsil" & input$domain_map1 == "Natural Hazards"){
    teh_haz_choices_c2 = (list(`River flooding` = list("Expected mortality from river floods (population count)", "Expected mortality from river floods (% of ADM population)", "Expected damage to built-up assets from river floods (hectares)", "Expected damage to built-up assets from river floods (% of ADM built-up area)", "Expected exposure of agricultural land to river floods (hectares)", "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
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
      "indicator_map1",    
      choices = teh_haz_choices_c2)
  }
})
})

#Updating spatial level based on selected domain: so that when domain goes to development, 
# District is selected by default
observeEvent(input$domain_map1,{
  if(input$domain_map1 == "Development Outcomes"){
    choices_pol_c1 = unique(data$polygon)[1]
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map1",
      choices = choices_pol_c1)
    
  }else{
    choices_pol_c2 = unique(data$polygon)
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map1",
      choices = choices_pol_c2)
  }
})


#leaflet Map1
output$double_map_1 <- renderLeaflet({
  
   leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>% 
    addProviderTiles(providers$CartoDB, group = "CARTO") %>% 
    setView(lng=70, lat = 30, zoom = 5)  %>% 
     syncWith("combined_map")

})

#To render leafelt map before proxy observer updates
outputOptions(output, "double_map_1", suspendWhenHidden = FALSE)


#Labelling for  Map
labels_map1 <- reactive({
  if(input$domain_map1 == "Development Outcomes"){  
    paste0(glue::glue("<b>District</b>: { pak_shp_comp()$district } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 2)  }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "Tehsil" & input$domain_map1 == "Natural Hazards"){
    paste0(glue::glue("<b>Tehsil</b>: { pak_shp_comp()$tehsil } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 3) }"), " ", glue::glue("{ map_data1()$unit }"),  sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "District" & input$domain_map1 == "Natural Hazards"){
    paste0(glue::glue("<b>District</b>: { pak_shp_comp()$district } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 3) }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "District" & input$domain_map1 == "Relative Wealth Index"){
    paste0(glue::glue("<b>District</b>: { pak_shp_comp()$district } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 2) }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "Tehsil" & input$domain_map1 == "Relative Wealth Index"){
    paste0(glue::glue("<b>Tehsil</b>: { pak_shp_comp()$tehsil } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data1()$value, 2) }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }
})

#Map Leaflet

  pal_new1 <- reactive({
    req(unique(map_data1()$context) %in% c("negative", "positive"))
    if (unique(map_data1()$context) == "negative"){
      c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
    } else {
      c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
    }
    
  })
 
  #breaks defined
  #since no variation in coastal floods so using the previous coloring approach instead of quintiles
  breaks_map1 <- reactive({
    req(unique(map_data1()$context) %in% c("negative", "positive"))
    if(
      unique(map_data1()$indicator_1) == "Expected mortality from coastal floods (population count)"||
      unique(map_data1()$indicator_1) == "Expected mortality from coastal floods (% of ADM population)" ||
      unique(map_data1()$indicator_1) == "Expected increase of mortality from air pollution (% of ADM population)"
    ){
      
      seq(min(map_data1()$value),
          max(map_data1()$value),
          (max(map_data1()$value)/3))
    }
    else if( unique(map_data1()$indicator_1) == "Expected damage to built-up assets from coastal floods (% of ADM built-up area)" ||
             unique(map_data1()$indicator_1) == "Expected damage to built-up assets from coastal floods (hectares)"){
      seq(min(map_data1()$value),
          max(map_data1()$value),
          (max(map_data1()$value)/2))
      
    } else if(unique(map_data1()$indicator_1) == "Expected mortality from river floods (% of ADM population)"||
              unique(map_data1()$indicator_1) == "Expected mortality from river floods (population count)"||
              unique(map_data1()$indicator_1) == "Expected damage to built-up assets from river floods (% of ADM built-up area)"){
      quantile(map_data1()$value, seq(0, 1, 1 / (7)), na.rm = TRUE) %>%
        unique()
      
    }else if(unique(map_data1()$indicator_1) == "Built-up assets exposed to medium or high landslide hazard (% of ADM built-up area)"||
             unique(map_data1()$indicator_1) == "Built-up assets exposed to medium or high landslide hazard (Hectares)"||
             unique(map_data1()$indicator_1) == "Population exposed to medium or high landslide hazard (% of ADM population)"||
             unique(map_data1()$indicator_1) == "Population exposed to medium or high landslide hazard (population count)"){
      quantile(map_data1()$value, seq(0, 1, 1 / (8)), na.rm = TRUE) %>%
        unique()
    } else{ quantile(map_data1()$value, seq(0, 1, 1 / (input$bins)), na.rm = TRUE) %>%
        unique()
    }
  })
  
  pal1 <- reactive({
    colorBin(palette = pal_new1(),
             bins= breaks_map1(), 
             na.color = "grey",
             domain = NULL,
             pretty = F,
             reverse=F)
    
  })
  
  pal_leg1 <- reactive({
    colorBin(palette = pal_new1(),
             bins= breaks_map1(), 
             na.color = "grey",
             domain = (map_data1()[,"value"]),
             pretty = F,
             reverse=F)
    
  })
 
  observe({
    leafletProxy("double_map_1", data= pak_shp_comp()) %>% 
    clearShapes() %>%
    addPolygons(label= labels_map1(),
                labelOptions = labelOptions(
                  style = list("font-weight"= "normal",   
                               padding= "3px 8px",
                               "color"= "black"), 
                  textsize= "10px",
                  direction = "auto",
                  opacity = 0.8
                ),
                fillColor =  ~pal1()(map_data1()$value),
                fillOpacity = 1,
                stroke = TRUE,
                color= "white",
                weight = 1,
                opacity = 0.9,
                fill = TRUE,
                dashArray = c(5,5),
                smoothFactor = 0.8,
                highlightOptions = highlightOptions(weight= 5,
                                                    fillOpacity = 1,
                                                    opacity= 1,
                                                    bringToFront = TRUE), 
                group = "Polygons") %>%
                addScaleBar("bottomleft")

    leafletProxy("double_map_1", data= map_data1()) %>% 
    clearControls() %>% 
    addLegend("bottomright",
              pal= pal_leg1(),
              values= map_data1()$value,
              title = 
                if(unique(map_data1()$unit) != ""){
                  glue("Legend", " ", "{ unique(map_data1()$unit)  }")
                }else{
                  "Legend"
                },
              opacity= 1,
              labFormat = labelFormat(
                between = "  :  ",
                digits = 2)
    )
})

# #Source Map1  
output$source_comp1 <- renderText({
  paste0(" MAP 1", 
         "\n",
         " Source: ", glue("{ unique(map_data1()$source) }"),
         "\n",
         " Definition:",  glue("{ unique(map_data1()$definition) }"))
})

# 
# #Screen shot
observeEvent(input$screenshot_comp,{
  screenshot(selector = "#double_map2") #filename = glue("{ input$indicator_comp1 }", "{ input$indicator_comp2 }", " ", "screenshot_comp"), selector = "#double_map", timer = 1
})

