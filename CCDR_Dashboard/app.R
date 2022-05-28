
#Utils
library(shiny)
library(dplyr)
library(shinycssloaders)
library(leaflet)
# library(shinydashboard)



################################################################################
#Read in Data
# Shape File (Both District + Tehsil)
pak_shp <- readRDS("data/pak_shp.RDS")
#Data Set
data <- readRDS("data/data.RDS")


#Socio-Economic Indicators List
development_outcomes <- unique(data[["development_indicators"]]$indicator)  

#Natural Hazard Option List
hazards_options <- unique(data[["hazards"]]$indicator)
                              
# Spatial Level
spatial_level <- c("District" = "district_shp", "Tehsil" = "tehsil_shp")

#Data domain
data_type <- c("Natural Hazards" = "hazards", "Development Outcomes" = "development_indicators")

################################################################################

#User Interface
################################################################################
ui <- navbarPage("CLIMATE Dashboard",
                 
                  # header= tagList(
                 #   useShinydashboard()
                 # ),
                 tabPanel("INTERACTIVE MAPS",
                          # bootstrapPage(),
                          # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                          tags$style(type = 'text/css', '#maps {height: calc(90vh - 80px) !important;}', style= 'padding-top:0px;'),
                          leafletOutput("maps") %>%
                            withSpinner(),
                          br(),
                          tags$head(tags$style("#source_map{color:black; font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc; }")),
                          verbatimTextOutput("source_map"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE,
                                        draggable = TRUE, bottom = "auto", right = "auto", left = 70, top = 80,
                                        width = 240, height = "auto",
                                        style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                        
                                      selectInput("domain_map",
                                                  "Choose Data Type",
                                                  choices = data_type,
                                                  selectize = F),
                                      
                                      # conditionalPanel(
                                      # condition = "input.domain_map == 'Natural Hazards'",
                                       selectInput("polygon_map",
                                               "Choose Spatial Level",
                                               choices = spatial_level,
                                               selectize = F),
                                       # ),
                                        
                                        selectInput("indicator_map",
                                               "Choose Indicator",
                                               choices = development_outcomes,
                                               selectize = F),
                                        # radioButtons("pallettes_fed", ("Change Color Scheme"), inline = TRUE, choices = list("Values"  = "pallette_fed1",
                                        # h6(tags$b(tags$em("Use this button to download the data underlying the current on-screen map"))),
                                        downloadButton("mapdata", "Data", class= "btn-sm"),
                                        actionButton("screenshot", "Image",class="btn-sm", icon=icon("camera")),
                                        actionButton("help_map", "Help", icon= icon('question-circle'), class ="btn-sm"),
                                        br()
                          )
                 )
)
                 

################################################################################

#Server
################################################################################
################################################################################
server <- function(input, output, session) {

#Main Maps
source(file.path("maps.R"), local = TRUE)
  
}
################################################################################

################################################################################
################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
################################################################################