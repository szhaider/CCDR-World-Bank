
#Utils
library(shiny)
library(dplyr)


################################################################################
#Read in Data
# Shape File (Both District + Tehsil)
pak_shp <- readRDS("data/pak_shp.RDS")
#Data Set
data <- readRDS("data/data.RDS")

################################################################################

#User Interface


#Server


# Run the application 
shinyApp(ui = ui, server = server)
