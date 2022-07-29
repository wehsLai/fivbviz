clipboardUI <- function(id) {
  ns <- NS(id)
  fillRow(
    flex = c(NA, 1),
    tagList(
      prettyCheckbox(ns("selected"), label = "Selected", value = TRUE, status = "primary", icon = icon("check")),
      prettyCheckbox(ns("shortName"), label = "Team Name", value = TRUE, status = "primary", icon = icon("check")),
      prettyCheckbox(ns("numShirt"), label = "No Shirt", value = TRUE, status = "primary", icon = icon("check")),
      actionButton(ns("copy"), label = "Copy", icon = icon("fa-light fa-copy"), `data-clipboard-target` = sprintf("#%s", ns("board"))),
      # trigger clipboard.js, place it at the end of the column
      uiOutput(ns("clip"), style = "visibility: hidden")
    ),
      verbatimTextOutput(ns("board"), placeholder = TRUE)
  )
}

clipboardServer <- function(id) {
  moduleServer(id, function(input, output, session) {
      
    clipVal <- reactiveVal(textRoster(rv$ds$players, input$selected, input$shortName, input$numShirt))

    output$board <- renderText({
      clipVal()
    })
    
    output$clip <- renderUI({
      rclipButton(
        inputId = "clipN",
        label = NULL,
        clipText = ""
      )
    })
  })
}
