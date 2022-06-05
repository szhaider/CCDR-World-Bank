#Comparison Maps
##############################################.        
#### Modal  ----
###############################################.
observeEvent(input$help_comp, {
  showModal(modalDialog(
    title = "How to use these maps",
    p("These maps offer comparison (spatial correlation) between selected CCDR-Pakistan indicators"),
    # p("MAP1 refers to"),
    p("These maps give district level estimates of CCDR-Pakistan indicators over the selected filters"),
    p("All Development Indicators are rounded to 2 decimal points"),
    p("All Natural Hazards Indicators are rounded to 3 decimal points"),
    p("Expect the color mapping to change with the context of  the selected indicators - e.g. Poverty (High) = Red whereas; Access to improved toilet facilities (High) = Blue"),
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
  data %>% 
    filter(polygon   ==  input$polygon_map1,
           domain    ==  input$domain_map1,
           indicator_1 ==  input$indicator_map1)
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
  }else {
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = hazards_options)
  }
})

observeEvent(input$polygon_map1,{
  if(input$polygon_map1 == "District" & input$domain_map1 == "Natural Hazards"){
    dis_haz_choices_c1 =  hazards_options[-c(24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = dis_haz_choices_c1
    )
  }else if(input$polygon_map1 == "Tehsil" & input$domain_map1 == "Natural Hazards"){
    teh_haz_choices_c2 =  hazards_options[-c(1,2,3)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map1",    
      choices = teh_haz_choices_c2)
  }
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


#Updating Indicators for 


#Labelling for  Map

labels_map1 <- reactive({
  if(input$domain_map1 == "Development Outcomes"){  
    paste0(glue::glue("<b>District</b>: { pak_shp_comp()$district } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), " ", glue::glue("{ round(map_data1()$value, 2)  }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "Tehsil" & input$domain_map1 == "Natural Hazards"){
    paste0(glue::glue("<b>Tehsil</b>: { pak_shp_comp()$tehsil } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), " ", glue::glue("{ round(map_data1()$value, 3) }"), " ", glue::glue("{ map_data1()$unit }"),  sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map1 == "District" & input$domain_map1 == "Natural Hazards"){
    paste0(glue::glue("<b>District</b>: { pak_shp_comp()$district } </br>"), glue::glue("<b> { map_data1()$indicator_1 }: </b>"), " ", glue::glue("{ round(map_data1()$value, 3) }"), " ", glue::glue("{ map_data1()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }
})


#Map Leaflet
comp_map1 <- reactive({
  
  pal_new1 <- reactive({
    req(unique(map_data1()$context) %in% c("negative", "positive"))
    if (unique(map_data1()$context) == "negative"){
      c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
    } else {
      c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
    }
    
  })
  
  pal1 <- reactive({
    colorBin(palette = pal_new1(), 
             bins= 7, na.color = "grey",  
             domain = map_data1()$value , 
             pretty = T,
             reverse=F)
    
  })
  
  leaflet(pak_shp_comp(), options = leafletOptions(zoomSnap = 0.20, 
                                               zoomDelta = 0.20)) %>% 
    addProviderTiles(providers$CartoDB, group = "CARTO") %>% 
    addProviderTiles(providers$Esri , group = "ESRI") %>%
    # addMiniMap() %>% 
    
    # addProviderTiles(providers$Stamen.Terrain,
    #                  options = tileOptions(minZoom = 0,
    #                                        maxZoom = 13),
    #                  group = "ST Terrain") %>%
    # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    # addProviderTiles(providers$Esri.WorldImagery , group = "ESRI IMG") %>% 
    # addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012 , group = "NASA Nightlights") %>% 
    
    setView(lng=71.5, lat = 29.5, zoom = 4.8) %>% 
    addPolygons(label= labels_map1(),
                labelOptions = labelOptions(
                  style = list("font-weight"= "normal",   
                               padding= "3px 8px",
                               "color"= "black"), 
                  textsize= "15px",
                  direction = "auto"
                ),
                fillColor =  ~pal1()(map_data1()$value),
                fillOpacity = 1,
                stroke = TRUE,
                color= "white",
                weight = 1,
                opacity = 0.7,
                fill = TRUE,
                dashArray = NULL,
                smoothFactor = 0.5,
                highlightOptions = highlightOptions(weight= 5,
                                                    fillOpacity = 1,
                                                    opacity= 1,
                                                    bringToFront = TRUE), 
                group = "Polygons") %>%
    
    addLayersControl(baseGroups = c("CARTO", "ESRI"),
                     # overlayGroups = c("Polygons"),
                     options = layersControlOptions(collapsed = TRUE)) %>% 
    
    addScaleBar("bottomleft") %>%
    # addMeasure("bottomleft") %>% 
    addLegend("bottomright",
              pal= pal1(),
              values= ~map_data1()$value,
              title = 
                if(unique(map_data1()$unit) != ""){
                  glue("Legend", " ", "({ unique(map_data1()$unit)  })")
                }else{
                  "Legend"
                },
              opacity= 1,
    )
})

#Map2

#shape file used
pak_shp_comp2 <-  reactive({
  pak_shp[[input$polygon_map2]]
})

#Development outcomes Data MAP1
map_data2 <- reactive({
  data %>% 
    filter(polygon   ==  input$polygon_map2,
           domain    ==  input$domain_map2,
           indicator_1 ==  input$indicator_map2)
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
      choices = hazards_options)
  }
})

observeEvent(input$polygon_map2,{
  if(input$polygon_map2 == "District" & input$domain_map2 == "Natural Hazards"){
    dis_haz_choices_c3 =  hazards_options[-c(24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map2",    
      choices = dis_haz_choices_c3
    )
  }else if(input$polygon_map2 == "Tehsil" & input$domain_map2 == "Natural Hazards"){
    teh_haz_choices_c4 =  hazards_options[-c(1,2,3)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map2",    
      choices = teh_haz_choices_c4)
  }
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
      "polygon_map1",
      choices = choices_pol_c4)
  }
})


#Updating Indicators for 


#Labelling for  Map

labels_map2 <- reactive({
  if(input$domain_map2 == "Development Outcomes"){  
    paste0(glue::glue("<b>District</b>: { pak_shp_comp2()$district } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), " ", glue::glue("{ round(map_data2()$value, 2)  }"), " ", glue::glue("{ map_data2()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map2 == "Tehsil" & input$domain_map1 == "Natural Hazards"){
    paste0(glue::glue("<b>Tehsil</b>: { pak_shp_comp2()$tehsil } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), " ", glue::glue("{ round(map_data2()$value, 3) }"), " ", glue::glue("{ map_data2()$unit }"),  sep = "") %>% 
      lapply(htmltools::HTML) 
  }else if(input$polygon_map2 == "District" & input$domain_map1 == "Natural Hazards"){
    paste0(glue::glue("<b>District</b>: { pak_shp_comp2()$district } </br>"), glue::glue("<b> { map_data2()$indicator_1 }: </b>"), " ", glue::glue("{ round(map_data2()$value, 3) }"), " ", glue::glue("{ map_data2()$unit }"), sep = "") %>% 
      lapply(htmltools::HTML) 
  }
})


#Map Leaflet
comp_map2 <- reactive({
  
  pal_new2 <- reactive({
    req(unique(map_data2()$context) %in% c("negative", "positive"))
    if (unique(map_data2()$context) == "negative"){
      c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
    } else {
      c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
    }
    
  })
  
  pal2 <- reactive({
    colorBin(palette = pal_new2(), 
             bins= 7, na.color = "grey",  
             domain = map_data2()$value , 
             pretty = T,
             reverse=F)
    
  })
  
  leaflet(pak_shp_comp2(), options = leafletOptions(zoomSnap = 0.20, 
                                                   zoomDelta = 0.20)) %>% 
    addProviderTiles(providers$CartoDB, group = "CARTO") %>% 
    addProviderTiles(providers$Esri , group = "ESRI") %>%
    # addMiniMap() %>% 
    
    # addProviderTiles(providers$Stamen.Terrain,
    #                  options = tileOptions(minZoom = 0,
    #                                        maxZoom = 13),
    #                  group = "ST Terrain") %>%
    # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    # addProviderTiles(providers$Esri.WorldImagery , group = "ESRI IMG") %>% 
    # addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012 , group = "NASA Nightlights") %>% 
    
    setView(lng=71.5, lat = 29.5, zoom = 4.8) %>% 
    addPolygons(label= labels_map2(),
                labelOptions = labelOptions(
                  style = list("font-weight"= "normal",   
                               padding= "3px 8px",
                               "color"= "black"), 
                  textsize= "15px",
                  direction = "auto"
                ),
                fillColor =  ~pal2()(map_data2()$value),
                fillOpacity = 1,
                stroke = TRUE,
                color= "white",
                weight = 1,
                opacity = 0.7,
                fill = TRUE,
                dashArray = NULL,
                smoothFactor = 0.5,
                highlightOptions = highlightOptions(weight= 5,
                                                    fillOpacity = 1,
                                                    opacity= 1,
                                                    bringToFront = TRUE), 
                group = "Polygons") %>%
    
    addLayersControl(baseGroups = c("CARTO", "ESRI"),
                     # overlayGroups = c("Polygons"),
                     options = layersControlOptions(collapsed = TRUE)) %>% 
    
    addScaleBar("bottomleft") %>%
    # addMeasure("bottomleft") %>% 
    addLegend("bottomright",
              pal= pal2(),
              values= ~map_data2()$value,
              title = 
                if(unique(map_data2()$unit) != ""){
                  glue("Legend", " ", "({ unique(map_data2()$unit)  })")
                }else{
                  "Legend"
                },
              opacity= 1,
    )
})


#Rendering synced maps in comparison MAPS
output$double_map <- renderUI({
  sync(comp_map1(), comp_map2(), ncol=2, no.initial.sync = T)
})    

#Source Map1  
output$source_comp1 <- renderText({
  paste0("MAP 1", "\n", 
         "Source: CCDR-Pakistan"
         # , "\n",
         # unique(d_c1()$indicator)
  )
})  
#Source Map 2
output$source_comp2 <- renderText({
  paste0("MAP 2", "\n", 
         "Source: CCDR-Pakistan")
         # ,"\n", 
         # "Definition: ",unique(d_c2()$definition)
  })


#Screen shot
observeEvent(input$screenshot_comp,{
  screenshot(filename = glue("{ input$indicator_comp1 }", "{ input$indicator_comp2 }", " ", "screenshot_comp"), selector = "#double_map", timer = 1)
})