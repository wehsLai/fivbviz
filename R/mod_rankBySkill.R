rankBySkillChartUI <- function(id) {
  ns <- NS(id)
  fillRow(
    plotlyOutput(ns("chart"))
  )
}

rankBySkillTableUI <- function(id) {
  ns <- NS(id)
  fillRow(
    tags$div(
      fluidRow(
        column(width = 9, align = "left", textOutput(ns("title"))),
        column(width = 2, align = "right", textOutput(ns("limit"))),
        column(width = 1, align = "right", tags$button("CSV", onclick = sprintf("Reactable.downloadDataCSV('%s')", ns("table"))))
      ),
      reactableOutput(ns("table"))
    )
  )
}

rankBySkillUI <- function(id) {
  ns <- NS(id)
  fillRow(
    flex = c(NA, 1),
    plotlyOutput(ns("chart")),
    tags$div(
      fluidRow(
        column(width = 9, align = "left", textOutput(ns("title"))),
        column(width = 2, align = "right", textOutput(ns("limit"))),
        column(width = 1, align = "right", tags$button("CSV", onclick = sprintf("Reactable.downloadDataCSV('%s')", ns("table"))))
      ),
      reactableOutput(ns("table"))
    )
  )
}

rankBySkillDfServer <- function(id, agg, type, isTeam = FALSE, showAll = TRUE, number = 10) {
  moduleServer(id, function(input, output, session) {
    f <- reactive({
      rankBySkillDf(agg, type, isTeam, showAll, number)
    })
    return(f())
  })
}

rankBySkillChartServer <- function(id, f, type, isTeam = FALSE, limit = 5) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    output$chart <- renderPlotly({
      renPlotly(id, f, type, rv$marktext, isTeam, limit)
    })
  })
}

rankBySkillTableServer <- function(id, f, type, isTeam = FALSE, limit = 0, pageSize = 20) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderText({
      renTitle(type)
    })

    output$limit <- renderText({
      renLimit(type)
    })

    output$table <- renderReactable({
      renTable(id, f, type, isTeam, limit, pageSize)
    })
  })
}
