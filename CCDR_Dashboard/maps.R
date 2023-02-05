#Proxy Mpas

#Main Interactive Maps
##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_map, {
  showModal(modalDialog(
    title = "How to use these maps",
    p("These maps give district level estimates of CCDR-Pakistan indicators over the selected filters"),
    p("All indicators are rounded to 2 decimal points"),
    # p("All Natural Hazards Indicators are rounded to 3 decimal points"),
    p("Expect the color mapping to reverse with the context of  the selected indicators - e.g. Poverty (High) = Red whereas; Access to improved toilet facilities (High) = Blue"),
    size = "m", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
})


###Code

#shape file used
pak_shp1 <-  reactive({
  pak_shp[[input$polygon_map]]
})

#Development outcomes Data
map_data <- reactive({
  data %>% 
    filter(polygon  %in%  input$polygon_map,
           domain    %in% input$domain_map,
           indicator_1 %in%  input$indicator_map)
})

#Updating Indicators based on Domain
observeEvent(input$domain_map,{
  if( input$domain_map == "Development Outcomes"){
    choices_dev =  data %>% 
      filter(domain == "Development Outcomes") %>% 
      distinct(indicator_1) %>% 
      pull(indicator_1)
    
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",    
      choices = choices_dev
    )}
      else if(input$domain_map == "Relative Wealth Index"){
        choices_dev =  data %>% 
          filter(domain == "Relative Wealth Index") %>% 
          distinct(indicator_1) %>% 
          pull(indicator_1)
        
        updateSelectInput(
          getDefaultReactiveDomain(),
          "indicator_map",    
          choices = choices_dev
        )
  }else {
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",    
      choices = indicator_listed
        # hazards_options[-11]
      )
  }
})

observeEvent(input$domain_map,{   #Latest to stop tehsil option to show up in districts when come back from development to hazards
observeEvent(input$polygon_map,{
  if(input$polygon_map == "District" & input$domain_map == "Natural Hazards"){
    dis_haz_choices =                           (list(`River flooding` = list("Expected mortality from river floods (population count)",
                                                                        "Expected mortality from river floods (% of ADM population)", 
                                                                        "Expected damage to built-up assets from river floods (hectares)",
                                                                        "Expected damage to built-up assets from river floods (% of ADM built-up area)", 
                                                                        "Expected exposure of agricultural land to river floods (hectares)",
                                                                        "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
                                                `Coastal flooding` = list("Expected mortality from coastal floods (population count)", 
                                                                          "Expected damage to built-up assets from coastal floods (hectares)", 
                                                                          "Expected damage to built-up assets from coastal floods (% of ADM built-up area)"),   #"Expected mortality from coastal floods (% of ADM population)",
                                                `Landslides` = list("Population exposed to medium or high landslide hazard (population count)",
                                                                    "Population exposed to medium or high landslide hazard (% of ADM population)",
                                                                    "Built-up assets exposed to medium or high landslide hazard (Hectares)", 
                                                                    "Built-up assets exposed to medium or high landslide hazard (% of ADM built-up area)"),
                                                `Drought` = list("Frequency of agricultural drought stress affecting at least 30% of arable land during Season 1/Kharif (percentage of historical period 1984-2022)",
                                                                 "Frequency of agricultural drought stress affecting at least 30% of arable land during Season 2/Rabi (percentage of historical period 1984-2022)" ),
                                                `Heat stress` = list("Expected exposure to heat stress (population count)",
                                                                     "Expected exposure to heat stress (% of ADM population)"),
                                                `Air pollution`= list("Expected increase of mortality from air pollution (population count)", 
                                                                      "Expected increase of mortality from air pollution (% of ADM population)"),
                                                `Demography` = list("District Population"),
                                                `Agriculture & Built-up Area` = list("Built-up area extent (Ha)", "Agricultural land extent (Ha)")))
    
      # hazards_options[-c(11, 24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",    
      choices = dis_haz_choices,
       selected = dis_haz_choices$`Agriculture & Built-up Area`[[2]]
    )
  }else if(input$polygon_map == "Tehsil" & input$domain_map == "Natural Hazards"){
    teh_haz_choices =                    (list(`River flooding` = list("Expected mortality from river floods (population count)", "Expected mortality from river floods (% of ADM population)", "Expected damage to built-up assets from river floods (hectares)", "Expected damage to built-up assets from river floods (% of ADM built-up area)", "Expected exposure of agricultural land to river floods (hectares)", "Expected exposure of agricultural land to river floods (% of ADM agricultural land)"),
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
      "indicator_map",    
      choices = teh_haz_choices)
  }
})
})

#Updating spatial level based on selected domain: so that when domain goes to development, 
# District is selected by default
observeEvent(input$domain_map,{
  if(input$domain_map == "Development Outcomes"){
    choices_pol1 = unique(data$polygon)[1]
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map",
      choices = choices_pol1)
    
  }else{
    choices_pol2 = unique(data$polygon)
    
    updateSelectizeInput(
      getDefaultReactiveDomain(),
      "polygon_map",
      choices = choices_pol2)
  }
})

#Lealfet
output$maps <- renderLeaflet({
  # message("rendering local map")
  leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
    addProviderTiles(providers$Esri, group = "ESRI") %>%
    addProviderTiles(providers$CartoDB, group = "CARTO") %>%
    # addProviderTiles(providers$st , group = "Stadia") %>% 
    setView(lng=69.5, lat = 30, zoom = 5.2)
})

#Labelling for  Map

  
labels_map <- reactive({
  if(input$domain_map == "Development Outcomes"){  
    paste0(glue::glue("<b>District</b>: { pak_shp1()$district } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 2)  }"), " ", glue::glue("{ map_data()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map == "Tehsil" & input$domain_map == "Natural Hazards"){
    paste0(glue::glue("<b>Tehsil</b>: { pak_shp1()$tehsil } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 3) }"), " ", glue::glue("{ map_data()$unit }"),  sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map == "District" & input$domain_map == "Natural Hazards"){
    paste0(glue::glue("<b>District</b>: { pak_shp1()$district } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 3) }"), " ", glue::glue("{ map_data()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map == "District" & input$domain_map == "Relative Wealth Index"){
    paste0(glue::glue("<b>District</b>: { pak_shp1()$district } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 2) }"), " ", glue::glue("{ map_data()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML)
  }else if(input$polygon_map == "Tehsil" & input$domain_map == "Relative Wealth Index"){
    paste0(glue::glue("<b>Tehsil</b>: { pak_shp1()$tehsil } </br>"), glue::glue("<b> { map_data()$indicator_1 }: </b>"), "\n", glue::glue("{ round(map_data()$value, 2) }"), " ", glue::glue("{ map_data()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }
})

  pal_new <- reactive({
    req(unique(map_data()$context) %in% c("negative", "positive"))
    if (unique(map_data()$context) == "negative"){
      rev(colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(input$bins))
      # rev(brewer.pal(input$bins, "RdYlBu"))
       # c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
    } else {
      colorRampPalette(colors = c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c'), space = "Lab")(input$bins)
      # brewer.pal(input$bins, "RdYlBu")
      # c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
    }

  })
  
  
#breaks defined
  #since no variation in coastal floods so using the previous coloring approach instead of quintiles
  breaks <- reactive({
    req(unique(map_data()$context) %in% c("negative", "positive"))
    if(
     unique(map_data()$indicator_1) == "Expected mortality from coastal floods (population count)"||
     unique(map_data()$indicator_1) == "Expected mortality from coastal floods (% of ADM population)" ||
    
     # unique(map_data()$indicator_1) == "Expected exposure to heat stress (% of ADM population)"||
      unique(map_data()$indicator_1) == "Expected increase of mortality from air pollution (% of ADM population)"
     ){

      seq(min(map_data()$value),
          max(map_data()$value),
          (max(map_data()$value)/3))
    }
      else if( unique(map_data()$indicator_1) == "Expected damage to built-up assets from coastal floods (% of ADM built-up area)" ||
               unique(map_data()$indicator_1) == "Expected damage to built-up assets from coastal floods (hectares)"){
        seq(min(map_data()$value),
            max(map_data()$value),
            (max(map_data()$value)/2))
        
      } else if(unique(map_data()$indicator_1) == "Expected mortality from river floods (% of ADM population)"||
              unique(map_data()$indicator_1) == "Expected mortality from river floods (population count)"||
              unique(map_data()$indicator_1) == "Expected damage to built-up assets from river floods (% of ADM built-up area)"){
        quantile(map_data()$value, seq(0, 1, 1 / (7)), na.rm = TRUE) %>%
          unique()
        
      }else if(unique(map_data()$indicator_1) == "Built-up assets exposed to medium or high landslide hazard (% of ADM built-up area)"||
              unique(map_data()$indicator_1) == "Built-up assets exposed to medium or high landslide hazard (Hectares)"||
              unique(map_data()$indicator_1) == "Population exposed to medium or high landslide hazard (% of ADM population)"||
              unique(map_data()$indicator_1) == "Population exposed to medium or high landslide hazard (population count)"){
      quantile(map_data()$value, seq(0, 1, 1 / (8)), na.rm = TRUE) %>%
        unique()
    } else{ quantile(map_data()$value, seq(0, 1, 1 / (input$bins)), na.rm = TRUE) %>%
      unique()
    }
  })

  pal <- reactive ({
    colorBin(palette = pal_new(),
             bins= breaks(),
             na.color = "grey",
             domain = NULL,
               # (map_data()[,"value"]),
             pretty = F,
             reverse=T
    )

  })
 
  pal_leg <- reactive ({
      colorBin(palette = pal_new(),
               bins= breaks(),
               na.color = "grey",
               domain = map_data()[,"value"],
              pretty = FALSE,
               reverse=T
      )
  })
  
  observe({
    
  req(input$main_page == "main_page1")  
    
  leafletProxy("maps", data=pak_shp1()) %>% 
    clearShapes() %>% 
    # clearControls() %>% 
    # removeControl("legend") %>% 
    addPolygons(label= labels_map(),
                labelOptions = labelOptions(
                  style = list("font-weight"= "normal",   
                               padding= "3px 8px",
                               "color"= "black"), 
                  textsize= "12px",
                  direction = "auto",
                  opacity = 0.9
                  
                ),
                fillColor =  ~pal()(map_data()$value),
                fillOpacity = 1,
                stroke = TRUE,
                color= "white",
                weight = 1,
                opacity = 0.9,
                fill = TRUE,
                dashArray = c(5,5),
                smoothFactor = .8,
                highlightOptions = highlightOptions(weight= 5,
                                                    fillOpacity = 1,
                                                    opacity= 1,
                                                    bringToFront = TRUE), 
                group = "Polygons") %>%
    
    addLayersControl(baseGroups = c("CARTO", "ESRI", "Stadia"),
                     # overlayGroups = c("Polygons"),
                     options = layersControlOptions(collapsed = TRUE)) %>% 
    
    addScaleBar("bottomleft") %>%
    addMeasure("bottomleft")

    leafletProxy("maps", data= map_data()) %>%
      clearControls() %>%
      addLegend("bottomright",
              pal= pal_leg(),
              values= map_data()$value,
              title =
                if(unique(map_data()$unit) != ""){
                  glue("Legend", " ", "{ unique(map_data()$unit)  }")
                }else{
                  "Legend"
                },
              opacity= 1,
              labFormat = labelFormat(
                between = "  :  ",
                digits = 2,
                transform = function(x)  {
                  x
                  # paste0(min(x),":", max(x))
                  }
              )
              )

})


output$source_map <- renderText({
  paste(" Source: ", glue("{ unique(map_data()$source) }"), 
        "\n",
        "Definition: ", glue("{ unique(map_data()$definition) }"))
})

observeEvent(input$screenshot,{
  screenshot(filename = glue("{ input$screenshot }", " ", "screenshot"), id = "maps", scale = 0.90, timer = 1)
  
})


#Download data underlying the shown map
output$mapdata <- downloadHandler(
  filename = function(){
    paste(glue("{ input$indicator_map }"), "_", glue("{ input$domain_map }"), ".csv")
  },
  content = function(file){
    write.csv(map_data(), file) 
  }
)

