
#Utils
library(shiny)
library(dplyr)
library(tidyr)
# library(ggplot2)
# library(plotly)
# library(scales)
# library(forcats)
library(shinyscreenshot)
library(glue)
library(leaflet)
library(sf)
library(shinythemes)
library(shinycssloaders)
library(htmltools)
library(DT)
# library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(leafsync)
library(highcharter)
# remotes::install_github("Ebukin/devPTIpack")
library(devPTIpack)
library(maptools)
library(gpclib)
gpclibPermit()
# library(broom)




################################################################################
#Read in Data
# Shape File (Both District + Tehsil)
pak_shp <- readRDS("data/pak_shp.RDS")
#Data Set
data <- readRDS("data/data.RDS")

#PTI Geometries
pti_shps <-  readRDS("data/pak_geometries.rds") 

#PTI Metadata
pti_mtdt <- readRDS("data/pak_metadata_climate.RDS")


#Socio-Economic Indicators List
development_options <- data %>%
  filter(domain == "Development Outcomes") %>%
  distinct(indicator) %>%
  pull(indicator)

#Natural Hazard Option List
hazards_options <- data %>% 
  filter(domain == "Natural Hazards") %>% 
  distinct(indicator_1) %>% 
  pull(indicator_1)

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
                          
                          # bootstrapPage(theme = shinytheme("flatly")),
                          # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                          tags$style(type = 'text/css', '#maps {height: calc(98vh - 100px) !important;}', style= 'padding-top:0px;'),
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
                                      br(),
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
                                        br(),
                                      br(),
                                      # br(),
                          )
                 ),
                 tabPanel("PTI",
                 tabsetPanel(id = "pti_help",
                             type = c("hidden"),
                             # selected ="PTI1" ,
                             
                 tabPanel("PTI1", 
                          id="PTI1", 
                         
                          devPTIpack::mod_ptipage_twocol_ui(
                            id = "pti_mod", 
                            map_height = "calc(90vh)", 
                            side_width = "350px", 
                             wt_style = "zoom:1;", 
                            show_waiter = FALSE,
                            wt_dwnld_options = c("data", "weights"),
                            map_dwnld_options = c()),
                          
                          mainPanel(
                            verbatimTextOutput("text"),
                          )
                          
                 )
                 
                 )
                 ),
                 tabPanel("COMPARISON MAPS",
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
                              
                              selectInput("domain_map1",
                                          "Choose Domain for MAP1",
                                          choices = domain_options,
                                          selected = domain_options[1],
                                          selectize = F),   
                              conditionalPanel(
                                condition = "input.domain_map1 == 'Natural Hazards'",
                                selectInput("polygon_map1",
                                            "Choose Spatial Level for MAP1",
                                            choices = spatial_level,
                                            selected = spatial_level[1],
                                            selectize = F)
                              ),
                              selectInput("indicator_map1",
                                          "Choose Indicator for MAP1",
                                          choices = hazards_options,
                                          selectize = F),
                              selectInput("domain_map2",
                                          "Choose Domain for MAP2",
                                          choices = domain_options,
                                          selected = domain_options[1],
                                          selectize = F),   
                              conditionalPanel(
                                condition = "input.domain_map2 == 'Natural Hazards'",
                                selectInput("polygon_map2",
                                            "Choose Spatial Level for MAP2",
                                            choices = spatial_level,
                                            selected = spatial_level[1],
                                            selectize = F)
                              ),
                              selectInput("indicator_map2",
                                          "Choose Indicator for MAP2",
                                          choices = hazards_options,
                                          selectize = F),
                             
                              actionButton("screenshot_comp", "Image",class="btn-sm", icon = icon("camera")),
                              # actionButton("help_comp", "Help", icon= icon('question-circle'), class ="btn-sm"),
                            ),
                          
                            mainPanel(
                              width = 9,
                              fluidRow(
                                column(width = 12,
                                       offset = 0,
                                       style = 'padding-bottom:0px; padding-left:0px; padding-right:0px',
                                       tags$style(type = 'text/css', '#double_map {height: calc(60vh - 60px) !important;}'),
                              uiOutput("double_map") %>%
                              withSpinner()
                              )),
                              # br(),
                              # br(),
                              fluidRow(
                                column(6,
                                       offset = 0.5,
                                       style =
                                       "padding-top: 1mm;
                                        padding-left: 0mm;",
                                       tags$head(tags$style("#source_comp1{color:black;  font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc; }")),
                                       verbatimTextOutput("source_comp1")),
                                column(6,
                                       offset = 0,
                                       style = 
                                      "padding-top:1mm;   
                                       padding-left:1px;",
                                       tags$head(tags$style("#source_comp2{color:black; font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc; }")),
                                       verbatimTextOutput("source_comp2")

                                ))
                            )
                            )
                            
                            
                          
                          
                   
                 ),
                 tabPanel("GRAPHS",
                         
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
                            choices = unique(data$province)[-5],
                            selectize = F
                          ),
                          
                          selectInput(
                            "domain_bar",
                            "Choose Domain",
                            choices = unique(data$domain)
                          ),
                         
                          conditionalPanel(
                          condition = "input.domain_bar == 'Natural Hazards'",  
                          selectInput("polygon_bar",
                                      "Choose Spatial Level",
                                      choices = unique(data$polygon),
                                      selected = "District",
                                      selectize = F)
                          ),
                          
                          selectInput(
                            "indicator_bar",
                            "Choose Indicator",
                            choices = unique(data$indicator_1),
                            selectize = F
                          ),
                          actionButton("help_bar", "Help", icon= icon('question-circle'), class ="btn-sm"),
                         
                            ),
                          mainPanel(
                            # width = 8,
                            
                            highchartOutput('bar_chart',
                                            width = '820px',
                                            height = '500px'),
                            
                            fluidRow(
                              # column(width = 4,
                              #        h5(tags$strong("Units: "))),
                              column(width = 8,
                                     (uiOutput("labels_bar"))
                              )
                          )
                            
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
                                          choices = unique(data$indicator_1),
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
                   
                 ),
                 tabPanel("ABOUT",
                          mainPanel(
                            width = 12,
                            
                            
tags$p(tags$strong(h4("CLIMATE DASHBOARD"))),
hr(),
tags$p(tags$b("Pakistan is currently among the countries most affected by extreme weather events globally."),
"The long-term Climate Risk Index (CRI) ranks Pakistan as the 8th most at-risk country over 
the 2000-2019 period, with 173 extreme events recorded (CRI 2021). Global climate projections 
of temperature and precipitation, compounded by the degradation of the environment, due to land use 
patterns and unplanned urban development, indicate that", tags$b("even under the most optimistic global climate 
scenario, Pakistan will continue to disproportionally – and more frequently and 
intensely – suffer "), "from extreme weather events and long-term climatic changes."  ) , 

tags$p(tags$b("However, disaster risk is not uniform across space."), 
"It is a function of the probability and intensity with 
which a hazard occurs, and the exposure of people and assets to this hazard, both of which differ strongly across
geographies. Moreover, socioeconomic conditions, driving vulnerability, are also highly heterogenous at small scales, 
making communities and households more, or less, vulnerable in the face of climate change. The analytics in this 
dashboard", tags$b("zoom in to the level of granular administrative units, where investment decisions are made and local policy 
should be targeted.")),

tags$p(tags$b("This dashboard focuses on disaster risk from floods, heat stress, droughts, landslides, 
       and air pollution."), "These pose a critical environmental concern for Pakistan, and indeed 
       across large parts of South Asia. To assess and quantify the impact of these hazards, 
       whether extreme events or long-term climatic changes, 
       we look at", tags$b("three types of exposure: (1) population, 
       (2) built-up assets, and (3) agricultural land."), 
       "Where available, an impact function is added to the exposure variables, 
       to demonstrate the expected annual impact on population health in terms 
       of morbidity and mortality, the potential damage to built-up assets and to 
       agricultural land. Vulnerability is then captured by a series of", 
       tags$b("socioeconomic indicators and development outcomes, tailored to specific hazards."),
       tags$hr(),
       
       tags$p(tags$em(tags$b("For further information and questions or suggestions, please reach out to:"))),
       
       tags$p("Ghazala Mansuri, Lead Economist, ESAPV  -",  tags$a("gmansuri@worldbank.org")),
       tags$p("Moritz Meyer, Senior Economist, ESAPV   -",  tags$a("mmeyer3@worldbank.org")),
       tags$p("Lander Bosch, Regional Geographer/YP    -",  tags$a("lbosch@worldbank.org")),
       tags$p("Mattia Amadio, Research Analyst         -",  tags$a("mamadio@worldbank.org")),
       tags$p("Henrik Fisser, Research Analyst         -",  tags$a("ghfisser@worldbank.org")),
       tags$p("Vincent Mariathanasan, Research Analyst -",  tags$a("vmariathanasan@worldbank.org")),
       tags$p("Maham Khan, Research Analyst            -",  tags$a("mkhan57@worldbank.org")),
       tags$p("Zeeshan Haider, Research Analyst        -",  tags$a("shaider7@worldbank.org")),

                            
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
#Main Landing page  
# source(file.path("main_landing_page.R"), local = TRUE)  
################################################################################
#Main Maps
source(file.path("maps.R"), local = TRUE)
################################################################################
  
################################################################################
#Comparison Maps
source(file.path("comparison_maps.R"), local = TRUE)  
################################################################################
#Bar Charts
source(file.path('bar_charts.R'), local = TRUE)  
################################################################################
#Main Tables
source(file.path("tables.R"), local = TRUE)  

################################################################################
################################################################################
#PTI Help
source(file.path("PTI_Help.R"), local= TRUE)
#PTI Server Side
  mod_ptipage_newsrv(
    id = "pti_mod",
    inp_dta = reactive(pti_mtdt),
    shp_dta = reactive(pti_shps),
    show_waiter = FALSE

  )
}
################################################################################

################################################################################
################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
################################################################################