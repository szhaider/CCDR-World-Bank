
#Utils
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(forcats)
library(shinyscreenshot)
library(glue)
library(leaflet)
library(sf)
library(shinythemes)
library(shinycssloaders)
library(htmltools)
library(DT)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
# library(leafsync)
library(highcharter)
# library(broom)



################################################################################
#Read in Data
# Shape File (Both District + Tehsil)
pak_shp <- readRDS("data/pak_shp.RDS")
#Data Set
data <- readRDS("data/data.RDS")


#Socio-Economic Indicators List
# development_options <- data %>% 
#   filter(domain == "Development Outcomes") %>% 
#   distinct(indicator) %>% 
#   pull(indicator)

#Natural Hazard Option List
hazards_options <- data %>% 
  filter(domain == "Natural Hazards") %>% 
  distinct(indicator) %>% 
  pull(indicator)

#Domain 
domain_options <- data %>% 
  distinct(domain) %>% 
  pull(domain)
                              
# Spatial Level
spatial_level <- unique(data$polygon)

#Listed Indicator Options
# indicator_listed = (list(`Natural Hazards` = list("Original Budget", "Original Budget Per Capita", "Original Budget Per Poor Person"),
#                           `Final Budget` = list("Final Budget", "Final Budget Per Capita","Final Budget Per Poor Person", "Ratio - Final to Original Budget"),
#                           `Actual Expenses` = list("Actual Expenses", "Actual Expenses Per Capita","Actual Expenses Per Poor Person",  "Ratio - Actual Expenses to Original Budget", "Ratio - Actual Expenses to Final Budget")))


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
                                        width = 250, height = "auto",
                                        style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                      
                                      selectInput("domain_map",
                                                    "Choose Domain",
                                                    choices = domain_options,
                                                    selected = domain_options[1],
                                                    selectize = F),   
                                      conditionalPanel(
                                      condition = "input.domain_map == 'Natural Hazards'",
                                      selectInput("polygon_map",
                                                    "Choose Spatial Level",
                                                    choices = spatial_level,
                                                    selected = spatial_level[1],
                                                    selectize = F)
                                      ),
                                      selectInput("indicator_map",
                                               "Choose Indicator",
                                               choices = hazards_options,
                                               selectize = F),
                                        # radioButtons("pallettes_fed", ("Change Color Scheme"), inline = TRUE, choices = list("Values"  = "pallette_fed1",
                                        # h6(tags$b(tags$em("Use this button to download the data underlying the current on-screen map"))),
                                        downloadButton("mapdata", "Data", class= "btn-sm"),
                                        actionButton("screenshot", "Image",class="btn-sm", icon=icon("camera")),
                                        actionButton("help_map", "Help", icon= icon('question-circle'), class ="btn-sm"),
                                        br()
                          )
                 ),
                 tabPanel("Graphs",
                         
                          sidebarLayout(
                            sidebarPanel(width= 3,
                                         style = "background-color: white;
                               opacity: 0.85;  
                               padding: 20px 20px 20px 20px;
                               margin: auto;
                               border-radius: 5pt;
                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                               padding-bottom: 2mm;
                               padding-top: 1mm;",
                           
                                                       
                          selectInput(
                            "province_bar",
                            "Choose Province",
                            choices = unique(data$province),
                            selectize = F
                          ),
                          
                          selectInput(
                            "domain_bar",
                            "Choouose Domain",
                            choices = unique(data$domain)
                          ),
                          
                          selectInput(
                            "indicator_bar",
                            "Choose Indicator",
                            choices = unique(data$indicator),
                            selectize = F
                          )),
                          mainPanel(
                            
                            highchartOutput('bar_chart')
                          )
                          )
                          
                          ),
                 tabPanel("TABLES",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              style = "background-color: white;
                               opacity: 0.85;  
                               padding: 20px 20px 20px 20px;
                               margin: auto;
                               border-radius: 5pt;
                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                               padding-bottom: 2mm;
                               padding-top: 1mm;",
                              
                              selectInput("table_province",
                                          "Choose Province",
                                          choices = unique(data$province),
                                          selectize = F),
                              
                              selectInput("table_domain",
                                          "Choose Domain",
                                          choices = unique(data$domain),
                                          selectize = F),
                              conditionalPanel(
                              condition = "input.table_domain == 'Natural Hazards'",
                              selectInput("table_polygon",
                                          "Choose Spatial Level",
                                          choices = unique(data$polygon),
                                          selectize = F)
                              ),
                              selectInput("table_indicator",
                                          "Choose Indicator",
                                          choices = unique(data$indicator),
                                          selectize = F),
                              
                              verbatimTextOutput("source_table"),
                              tags$head(tags$style("#source_table{color:black; font-size:12px; font-style:italic; 
               overflow-y:scroll; max-height: 120px; background: #ffe6cc;}")),
                              br(),
                              downloadButton("downloadtable",
                                             "Save",
                                             type= "default", 
                                             class="btn-sm"),
                              actionButton("help_tables", "Help", icon= icon('question-circle'), class ="btn-sm")
                            
                            ),
                            mainPanel(
                              width=9,
                              dataTableOutput("tables_main")
                            )
                          )
                   
                 )
)
                 

################################################################################

#Server
################################################################################
################################################################################
server <- function(input, output, session) {

################################################################################
#Main Maps
source(file.path("maps.R"), local = TRUE)
################################################################################
#Bar Charts
source(file.path('bar_charts.R'), local = TRUE)  
################################################################################
#Main Tables
source(file.path("tables.R"), local = TRUE)  
}
################################################################################

################################################################################
################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
################################################################################