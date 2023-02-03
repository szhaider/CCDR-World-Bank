#Comparison Map 2

#Map2

#shape file used
pak_shp_comp2 <-  reactive({
  pak_shp[[input$polygon_map2]]
})

map_data2 <- reactive({
  # req(input$domain_map2 == "Natural Hazards" || input$domain_map2 =="Development Outcomes")
  data %>%
    filter(polygon   %in%  input$polygon_map2,
           domain    %in%  input$domain_map2,
           indicator_1 %in%  input$indicator_map2)
})

#Updating Indicators based on Domain
observeEvent(input$domain_map2,{
  if( input$domain_map2 == "Development Outcomes"){
    choices_dev_c2 =  data %>%
      filter(domain == "Development Outcomes") %>%
      distinct(indicator_1) %>%
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map2",
      choices = choices_dev_c2
    )
  }else {
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map2",
      choices = hazards_options[-11]
      )
  }
})

observeEvent(input$domain_map2,{
observeEvent(input$polygon_map2,{
  if(input$polygon_map2 == "District" & input$domain_map2 == "Natural Hazards"){
    dis_haz_choices_c3 = (list(`River flooding` = list("Expected mortality from river floods (population count)", "Expected mortality from river floods (% of ADM population)", "Expected damage to built-up assets from river floods (hectares)", "Expected damage to built-up assets from river floods (% of ADM built-up area)", "Expected exposure of agricultural land to river floods (hectares)", "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
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
      "indicator_map2",
      choices = dis_haz_choices_c3,
      selected =  dis_haz_choices_c3$`Agriculture & Built-up Area`[[2]]
    )
  }else if(input$polygon_map2 == "Tehsil" & input$domain_map2 == "Natural Hazards"){
    teh_haz_choices_c4 =  (list(`River flooding` = list("Expected mortality from river floods (population count)", "Expected mortality from river floods (% of ADM population)", "Expected damage to built-up assets from river floods (hectares)", "Expected damage to built-up assets from river floods (% of ADM built-up area)", "Expected exposure of agricultural land to river floods (hectares)", "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
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
      "indicator_map2",
      choices = teh_haz_choices_c4)
  }
})
})

#Updating spatial level based on selected domain: so that when domain goes to development,
# District is selected by default
observeEvent(input$domain_map2,{
  if(input$domain_map2 == "Development Outcomes"){
    choices_pol_c3 = unique(data$polygon)[1]
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map2",
      choices = choices_pol_c3)
    
  }else{
    choices_pol_c4 = unique(data$polygon)
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map2",
      choices = choices_pol_c4)
  }
})

#Leaflet map2
output$double_map_2 <- renderLeaflet({
  leaflet(options = leafletOptions(zoomSnap = 0.20, 
                                   zoomDelta = 0.20)) %>% 
    addProviderTiles(providers$CartoDB, group = "CARTO") %>% 
    # addProviderTiles(providers$Esri , group = "ESRI") %>%
    setView(lng=70, lat = 30, zoom = 5) %>% 
    syncWith("combined_map")
  
})


outputOptions(output, "double_map_2", suspendWhenHidden = FALSE)

# observeEvent(input$my_tab == "my_tab1",{
#Labelling for  Map2

  
  # req(input$domain_map2 == "Natural Hazards" || input$domain_map2 =="Development Outcomes")
  labels_map2 <- reactive({
    if(input$domain_map2 == "Development Outcomes"){
      paste0(glue::glue("<b>District</b>: { pak_shp_comp2()$district } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data2()$value, 2)  }"), " ", glue::glue("{ map_data2()$unit }"), sep = "") %>%
        lapply(htmltools::HTML)
    }else if(input$polygon_map2 == "Tehsil" & input$domain_map2 == "Natural Hazards"){
      paste0(glue::glue("<b>Tehsil</b>: { pak_shp_comp2()$tehsil } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data2()$value, 3) }"), " ", glue::glue("{ map_data2()$unit }"),  sep = "") %>%
        lapply(htmltools::HTML)
    }else if(input$polygon_map2 == "District" & input$domain_map2 == "Natural Hazards"){
      paste0(glue::glue("<b>District</b>: { pak_shp_comp2()$district } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data2()$value, 3) }"), " ", glue::glue("{ map_data2()$unit }"), sep = "") %>%
        lapply(htmltools::HTML)
    }
  })
  
  pal_new2 <- reactive({
    req(unique(map_data2()$context) %in% c("negative", "positive"))
    if (unique(map_data2()$context) == "negative"){
      c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
    } else {
      c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
    }
    
  })
  
  breaks_map2 <- reactive({
    req(unique(map_data2()$context) %in% c("negative", "positive"))
    if(
    #   unique(map_data2()$indicator_1) == "Expected mortality from coastal floods (population count)"||
    #   unique(map_data2()$indicator_1) == "Expected mortality from coastal floods (% of ADM population)" ||
    #   unique(map_data2()$indicator_1) == "Expected damage to built-up assets from coastal floods (hectares)" ||
    #   unique(map_data2()$indicator_1) == "Expected damage to built-up assets from coastal floods (% of ADM built-up area)" ||
    #   unique(map_data2()$indicator_1) == "Expected exposure to heat stress (% of ADM population)"||
    #   # unique(map_data2()$indicator_1) == "Expected mortality from river floods (% of ADM population)" ||
    #   unique(map_data2()$indicator_1) == "Expected increase of mortality from air pollution (% of ADM population)"){
    #   
    #   seq(min(map_data2()$value), max(map_data2()$value), (max(map_data2()$value)/3))
    # } else {
    #   quantile(map_data2()$value, seq(0, 1, 1 / (5)), na.rm = TRUE) %>%
    #     unique()
    # }
      unique(map_data2()$indicator_1) == "Expected mortality from coastal floods (population count)"||
      unique(map_data2()$indicator_1) == "Expected mortality from coastal floods (% of ADM population)" ||
      unique(map_data2()$indicator_1) == "Expected increase of mortality from air pollution (% of ADM population)"
    ){
      
      seq(min(map_data2()$value),
          max(map_data2()$value),
          (max(map_data2()$value)/3))
    }
    else if( unique(map_data2()$indicator_1) == "Expected damage to built-up assets from coastal floods (% of ADM built-up area)" ||
             unique(map_data2()$indicator_1) == "Expected damage to built-up assets from coastal floods (hectares)"){
      seq(min(map_data2()$value),
          max(map_data2()$value),
          (max(map_data2()$value)/2))
      
    } else if(unique(map_data2()$indicator_1) == "Expected mortality from river floods (% of ADM population)"||
              unique(map_data2()$indicator_1) == "Expected mortality from river floods (population count)"||
              unique(map_data2()$indicator_1) == "Expected damage to built-up assets from river floods (% of ADM built-up area)"){
      quantile(map_data2()$value, seq(0, 1, 1 / (7)), na.rm = TRUE) %>%
        unique()
      
    }else if(unique(map_data2()$indicator_1) == "Built-up assets exposed to medium or high landslide hazard (% of ADM built-up area)"||
             unique(map_data2()$indicator_1) == "Built-up assets exposed to medium or high landslide hazard (Hectares)"||
             unique(map_data2()$indicator_1) == "Population exposed to medium or high landslide hazard (% of ADM population)"||
             unique(map_data2()$indicator_1) == "Population exposed to medium or high landslide hazard (population count)"){
      quantile(map_data2()$value, seq(0, 1, 1 / (8)), na.rm = TRUE) %>%
        unique()
    } else{ quantile(map_data2()$value, seq(0, 1, 1 / (input$bins)), na.rm = TRUE) %>%
        unique()
    }
  })
  
  pal2 <- reactive({
    colorBin(palette = pal_new2(),
             bins= breaks_map2(), 
             na.color = "grey",
             domain = NULL,
             pretty = F,
             reverse=F)
    
  })
  
  pal_leg2 <- reactive({
    colorBin(palette = pal_new2(),
             bins= breaks_map2(), 
             na.color = "grey",
             domain = map_data2()$value ,
             pretty = F,
             reverse=F)
    
  })
  
  
  observe({ 
  # req(input$my_tab == "my_tab1")  
    
  leafletProxy("double_map_2", data= pak_shp_comp2()) %>%
    clearShapes() %>% 
    addPolygons(label= labels_map2(),
                labelOptions = labelOptions(
                  style = list("font-weight"= "normal",
                               padding= "3px 8px",
                               "color"= "black"),
                  textsize= "10px",
                  direction = "auto",
                  opacity = 0.8
                ),
                fillColor =  ~pal2()(map_data2()$value),
                fillOpacity = 1,
                stroke = TRUE,
                color= "white",
                weight = 1,
                opacity = 1,
                fill = TRUE,
                dashArray = NULL,
                smoothFactor = 0.5,
                highlightOptions = highlightOptions(weight= 5,
                                                    fillOpacity = 1,
                                                    opacity= 1,
                                                    bringToFront = TRUE),
                group = "Polygons") %>%
                addScaleBar("bottomleft") 
  
  leafletProxy("double_map_2", data = map_data2()) %>% 
    clearControls() %>% 
    addLegend("bottomright",
              pal= pal_leg2(),
              values= map_data2()$value,
              title =
                if(unique(map_data2()$unit) != ""){
                  glue("Legend", " ", "{ unique(map_data2()$unit)  }")
                }else{
                  "Legend"
                },
              opacity= 1,
              labFormat = labelFormat(
                between = "  :  ",
                digits = 2)
    )
})

# #Source Map 2
  output$source_comp2 <- renderText({
    paste0(" MAP 2", 
           "\n",
           " Source: ", glue("{ unique(map_data2()$source) }",),
           "\n",
           " Definition: ", glue("{ unique(map_data2()$definition) }"))
    # ,"\n",
    # "Definition: ",unique(d_c2()$definition)
  })
