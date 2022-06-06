#PTI Explanation

output$text <- renderPrint({
  observeEvent(input$pti_help, {
    showModal(modalDialog(
      title = tags$strong("Project Targeting Index (PTI)"),
      p(tags$em("Steps to calculate and render PTI scores")),
      p(tags$b("Step 1:"), "Name your PTI e.g. Test1"),
      p(tags$b("Step 2:"), "Assign weights to the desired indicators of the respective domains - User can select 'All 1' or 'All 0' for ease"),
      p(tags$b("Step 3:"), "Click 'Save and plot PTI' to render PTI scores on the map"),
      tags$hr(),
      p(tags$strong("What is PTI?")),
      p("PTI is a composite index - which can be used to identify areas in the country that need to be prioritized based on a data driven criterion"),
      p("PTI = z-scores calculated based on selected indicators and assigned weights"),
      p("z-score of indicator i:  z_i = (y_i - mean(y_i))/sd(y_i)"),
      p("Then computing the weighted sum of all indicators assigned"),
      p("Sigma_{i}[z_i * wt_i]; where wt_i is a weight assigned to indicator i"),
      p(tags$b("Higher value gets higher PTI score, thus higher importance for geographic targeting")),
      p(tags$b("Note:"), "Meta data has been prepared to reverse priority order for certain  indicators such as 'Housing with improved roof and wall material (PSLM 2014)'")
    ))
  })
})