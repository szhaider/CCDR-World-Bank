#PTI Explanation

# output$text <- renderPrint({
  observeEvent(input$pti_link_1, {
    showModal(modalDialog(
      title = tags$strong("What is the Project Targeting Index (PTI)?"),
      p("The Project Targeting Index (PTI) offers an evidence-based approach to inform spatial targeting of projects based on country strategies and objectives. It allows to ensure that the World Bank portfolio is well targeted to areas with the greatest needs and with greatest potential to maximize development benefits."),
      p("Using the PTI, project and country teams can match objectives with geographic project site selection. In this dashboard, users can calculate the Project Targeting Index for small administrative units, highlighting which units are in greatest need of intervention."), 
      p("This allows to determine where World Bank projects and funds could be targeted to close spatial disparities in welfare and development outcomes based on a set of parameters and indicators selected by the user.")
    ))
      })
  
  observeEvent(input$pti_link_2, {
    showModal(modalDialog(
      title = tags$strong("How is the PTI constructed, and how should the score be interpreted?"),
      p("Calculating the PTI is a four-step process:"), 
      p("1. Identify development objectives relevant to the project, intervention, or portfolio that can be measured objectively through data."), 
      p("2. Identify indicators pertaining to these specific objectives from the available set of indicators on the dashboard, at the relevant spatial level (districts and/or tehsils)."), 
      p("3. Construct a composite weighted index by aggregating the selected indicators to construct a composite index. The PTI standardizes all indicators to have mean and variance of 0 and 1 respectively. These standardized indicators are given weights chosen by the user, reflecting the relative importance of each variable. As the weight of an indicator increases, the geographic distribution of priority areas of PTI is more affected by that indicator. The dashboard then creates a map of priority areas based on the selected indicators and assigned weights."),  
      p("4. Construct a final PTI and corresponding map of priority areas. Five classes are shown with an equal number of administrative units, showing those areas in highest need of investment in red, down to the best-performing areas according to the selected indicators in dark blue. The PTI score is shown as a pop-up when hovering over the administrative unit and can be downloaded in Excel format. A higher PTI score indicates a higher priority area."),
      
      p("More information on the PTI and applications can be found in this",
      tags$b(tags$a(href="https://documents.worldbank.org/en/publication/documents-reports/documentdetail/993931596523347991/subnational-targeting-of-project-sites-using-project-targeting-index-pti", 
      "Poverty & Equity Methods Note")))
     
    ))
  })
 # })



# output$text <- renderPrint({
#   observeEvent(input$pti_help, {
#     showModal(modalDialog(
#       title = tags$strong("Project Targeting Index (PTI)"),
#       p(tags$em("Steps to calculate and render PTI scores")),
#       p(tags$b("Step 1:"), "Name your PTI e.g. Test1"),
#       p(tags$b("Step 2:"), "Assign weights to the desired indicators of the respective domains - User can select 'All 1' or 'All 0' for ease"),
#       p(tags$b("Step 3:"), "Click 'Save and plot PTI' to render PTI scores on the map"),
#       tags$hr(),
#       p(tags$strong("What is PTI?")),
#       p("PTI is a composite index - which can be used to identify areas in the country that need to be prioritized based on a data driven criterion"),
#       p("PTI = z-scores calculated based on selected indicators and assigned weights"),
#       p("z-score of indicator i:  z_i = (y_i - mean(y_i))/sd(y_i)"),
#       p("Then computing the weighted sum of all indicators assigned"),
#       p("Sigma_{i}[z_i * wt_i]; where wt_i is a weight assigned to indicator i"),
#       p(tags$b("Higher value gets higher PTI score, thus higher importance for geographic targeting")),
#       p(tags$b("Note:"), "Meta data has been prepared to reverse priority order for certain  indicators such as 'Housing with improved roof and wall material (PSLM 2014)'")
#     ))
#   })
# })