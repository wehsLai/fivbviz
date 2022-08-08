clipboardUI <- function(id) {
  ns <- NS(id)
  fillRow(
    flex = c(NA, 1),
    tagList(
      prettyCheckbox(ns("selected"), label = "Selected", value = TRUE, status = "primary", icon = icon("check")),
      prettyCheckbox(ns("shortName"), label = "Team Name", value = TRUE, status = "primary", icon = icon("check")),
      prettyCheckbox(ns("numShirt"), label = "No Shirt", value = TRUE, status = "primary", icon = icon("check")),
      prettyCheckbox(ns("latin1"), label = "latin1", value = TRUE, status = "primary", icon = icon("check")),
      actionButton(ns("copy"), label = "Copy", icon = fontawesome::fa_i(name = "copy"), `data-clipboard-target` = sprintf("#%s", ns("board"))),
      # trigger clipboard.js, place it at the end of the column
      uiOutput(ns("clip"), style = "visibility: hidden")
    ),
    verbatimTextOutput(ns("board"), placeholder = TRUE)
  )
}
clipBtnUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("clip"), style = "visibility: hidden")
}

clipboardServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    clipVal <- reactive({
      textRoster(rv$ds$players, input$selected, input$shortName, input$numShirt)
    })

    output$board <- renderText({
      # convert to latin1
      if (input$latin1) {
        iconv(clipVal(), "UTF8", "latin1")
      } else {
        clipVal()
      }
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
