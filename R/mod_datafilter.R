dataFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("round_pick"),
        label = "Round", choices = c(), selected = NULL,
        multiple = TRUE, options = list(`actions-box` = TRUE), width = "fit"
    ),
    actionBttn(ns("filter"), label = "Filter", style = "bordered", color = "primary", size = "sm"),
    verbatimTextOutput(ns("msg"), placeholder = TRUE)
  )
}

dataFilterServer <- function(id, showSeason, addPoolName = TRUE) {
  moduleServer(id, function(input, output, session) {
    observe({
      output$msg <- renderText(rv$marktext)
    })

    observe({
      updatePickerInput(session = session, inputId = "round_pick", choices = unique(rv$ds$matches$poolRoundName), selected = character(0))
    })

    pick <- reactive({
      input$round_pick
    })

    # filter by selected round
    observeEvent(input$filter, {
      waiter_show(html = waiting_screen)
      if (is.null(pick()) || identical(pick(), unique(rv$ds$matches$poolRoundName))) {
        rv$marktext <- paste0(ifelse(showSeason, paste0(rv$ds$tournament$season, " - "), ""), rv$ds$tournament$shortNameOrName)
        rv$fds <- rv$ds
      } else {
        if (addPoolName) {
          rv$marktext <- paste0(
            ifelse(showSeason, paste0(rv$ds$tournament$season, " - "), ""),
            rv$ds$tournament$shortNameOrName, " - ", paste0(pick(), collapse = ", ")
          )
        }
        if (!is.null(rv$ds) && all(map_lgl(rv$ds$statistics, ~nrow(.x) > 0)) == TRUE) {
          rv$fds$matches <- rv$ds$matches[rv$ds$matches$poolRoundName %in% pick(), ]
          rv$fds$statistics$Player <- rv$ds$statistics$Player[rv$ds$statistics$Player$noMatch %in% rv$fds$matches$no, ]
          rv$fds$statistics$Team <- rv$ds$statistics$Team[rv$ds$statistics$Team$noMatch %in% rv$fds$matches$no, ]
          rv$fds$statistics <- add_agg(rv$fds$statistics)
        }
      }
      waiter_hide()
    })
  })
}
