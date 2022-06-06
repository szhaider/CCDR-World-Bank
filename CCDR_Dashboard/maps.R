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
    filter(polygon   ==  input$polygon_map,
           domain    ==  input$domain_map,
           indicator_1 ==  input$indicator_map)
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
)
  }else {
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",    
      choices = hazards_options)
  }
})

observeEvent(input$polygon_map,{
  if(input$polygon_map == "District" & input$domain_map == "Natural Hazards"){
    dis_haz_choices =  hazards_options[-c(11, 24,25,26)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",    
      choices = dis_haz_choices,
      selected = dis_haz_choices[3]
    )
  }else if(input$polygon_map == "Tehsil" & input$domain_map == "Natural Hazards"){
    teh_haz_choices =  hazards_options[-c(1,2,3, 11)]
    updateSelectInput(
      getDefaultReactiveDomain(),
      "indicator_map",    
      choices = teh_haz_choices)
  }
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


#Updating Indicators for 


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
}
})


#Map Leaflet
main_map <- reactive({
  
  pal_new <- reactive({
      req(unique(map_data()$context) %in% c("negative", "positive"))
      if (unique(map_data()$context) == "negative"){
         c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
        # c("#313695", "#ffffbf", "#d73027")
      } else {
         c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
        # c( "#d73027"  , "#ffffbf", "#313695")
      }

    })

 pal <- reactive ({
    colorBin(palette = pal_new(),
    bins= 5,
    na.color = "grey",
               domain = map_data()$value,
               pretty = F,
               reverse=F
    )
               })
 # suppressWarnings(pal())

  # pal <- reactive({
  # if(unique(map_data()$context) == "negative"){
  #    colorBin(palette = "RdYlBu",
  #            bins= 6, na.color = "grey",
  #            domain = map_data()$value ,
  #            pretty = T,
  #            reverse= TRUE
  #                 )
  #   }else if(unique(map_data()$context) == "positive"){
  # colorBin(palette = "RdYlBu",
  #          bins= 6, na.color = "grey",
  #          domain = map_data()$value ,
  #          pretty = T,
  #          reverse=FALSE)
  #   }
  #   
  # })
  
  leaflet(pak_shp1(), options = leafletOptions(zoomSnap = 0.20, 
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
    
    setView(lng=69.5, lat = 30, zoom = 5.2) %>% 
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
                opacity = 1,
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
    addMeasure("bottomleft") %>% 
    addLegend("bottomright",
              pal= pal(),
              values= ~map_data()$value,
              title = 
                if(unique(map_data()$unit) != ""){
                glue("Legend", " ", "{ unique(map_data()$unit)  }")
                  }else{
                    "Legend"
                  },
              opacity= 1,
              labFormat = labelFormat(
              digits = 2)
              )
})

#Tehsil
# main_map_tehsil <- reactive({
#   
#   pal <- reactive({
#     colorBin(palette = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), 
#              bins= 7, na.color = "grey",  
#              domain = map_data_haz()$value , 
#              pretty = T)
#   })
#   
#   leaflet(pak_shp_tehsil(), options = leafletOptions(zoomSnap = 0.20, 
#                                                        zoomDelta = 0.20)) %>% 
#     addProviderTiles(providers$CartoDB, group = "CARTO") %>% 
#     addProviderTiles(providers$Esri , group = "ESRI") %>%
#     # addMiniMap() %>% 
#     
#     # addProviderTiles(providers$Stamen.Terrain,
#     #                  options = tileOptions(minZoom = 0,
#     #                                        maxZoom = 13),
#     #                  group = "ST Terrain") %>%
#     # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
#     # addProviderTiles(providers$Esri.WorldImagery , group = "ESRI IMG") %>% 
#     # addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012 , group = "NASA Nightlights") %>% 
#     
#     setView(lng=69, lat = 31, zoom = 5.2) %>% 
#     addPolygons(label= labels_map_haz(),
#                 labelOptions = labelOptions(
#                   style = list("font-weight"= "normal",   
#                                padding= "3px 8px",
#                                "color"= "black"), 
#                   textsize= "15px",
#                   direction = "auto"
#                 ),
#                 fillColor =  ~pal()(map_data_haz()$value),
#                 fillOpacity = 1,
#                 stroke = TRUE,
#                 color= "white",
#                 weight = 1,
#                 opacity = 0.7,
#                 fill = TRUE,
#                 dashArray = NULL,
#                 smoothFactor = 0.5,
#                 highlightOptions = highlightOptions(weight= 5,
#                                                     fillOpacity = 1,
#                                                     opacity= 1,
#                                                     bringToFront = TRUE), 
#                 group = "Polygons") %>%
#     
#     addLayersControl(baseGroups = c("CARTO", "ESRI"),
#                      # overlayGroups = c("Polygons"),
#                      options = layersControlOptions(collapsed = TRUE)) %>% 
#     
#     addMeasure() %>% 
#     addScaleBar("bottomright") %>%
#     addLegend("bottomright",
#               pal= pal(),
#               values= ~map_data_haz()$value,
#               title = "Legend",
#               opacity= 1,
#               # labFormat = labelFormat(prefix = "Rs.")
#     )
# })


#Rendering Maps (District And Tehsil)
output$maps <- renderLeaflet({
# if(input$polygon_map == "District"){
  main_map()
# }else{
#   main_map_tehsil()
# }
})

output$source_map <- renderText({
  paste("Source: World Bank CCDR Pakistan")
})

observeEvent(input$screenshot,{
  screenshot(filename = glue("{ input$screenshot }", " ", "screenshot"), selector = "#maps", scale = 0.90, timer = 1)
  
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

