tournamentsUI <- function(id) {
  ns <- NS(id)
  fillCol(
    height = "790px", flex = c(NA, 1),
    fluidRow(
      column(width = 1, actionBttn(ns("open"), label = "Open", style = "bordered", color = "primary", size = "sm")),
      column(width = 3, offset = 0, prettyCheckbox(ns("check"), label = "Show Season", value = TRUE, status = "primary", icon = icon("check")))
    ),
    reactableOutput(ns("table"))
  )
}

showSeasonVal <- function(id) {
  moduleServer(id, function(input, output, session) {
    # show Season in marktext
    showSeason <- reactive(input$check)
    return(showSeason)
  })
}

tournamentsServer <- function(id, showSeason) {
  moduleServer(id, function(input, output, session) {
    # get tournaments list
    tournaments <- reactive({
      pl <- list(Fields = "No Season Name StartDate EndDate Gender OrganizerType Status DeadlineO2 DeadlineO2bis")
      v_get_volley_tournament_list(parent = pl) %>% arrange(desc(startDate))
    })

    # render tournaments table
    output$table <- renderReactable({
      tl.colDef <- list(
        no = colDef(minWidth = 50, align = "left"),
        name = colDef(name = "Tournament", minWidth = 280),
        startDate = colDef(minWidth = 85),
        endDate = colDef(minWidth = 85)
      )

      reactable(
        tournaments(),
        columns = tl.colDef,
        defaultColDef = df.colDef,
        style = list(fontFamily = fontfamily, minWidth = 650),
        defaultPageSize = 20,
        resizable = TRUE,
        filterable = TRUE,
        highlight = TRUE,
        striped = TRUE,
        compact = TRUE,
        wrap = TRUE,
        selection = "single",
        onClick = "select",
        rowStyle = JS("function(rowInfo) {
                         if (rowInfo && rowInfo.selected) {
                           return { backgroundColor: '#d4d4d4', boxShadow: 'inset 2px 0 0 0 #ffa62d' }
                         }
                       }")
      )
    })

    # selected row
    sel_row <- reactive(getReactableState("table", "selected"))

    # open selected tournament
    observeEvent(input$open, {
      if (is.null(sel_row())) {
        # stop("Select one tournament")
      } else {
        waiter_show(html = waiting_screen)
        sel <- tournaments()[sel_row(), ]
        rv$ds <- get_tournament_data(sel$no)
        if (!is.null(rv$ds) && all(map_lgl(rv$ds$statistics, ~ nrow(.x) > 0)) == TRUE) {
          rv$ds$statistics <- add_agg(rv$ds$statistics)
        }
        rv$marktext <- paste0(ifelse(showSeason, paste0(rv$ds$tournament$season, " - "), ""), rv$ds$tournament$name)
        rv$fds <- rv$ds
        waiter_hide()
      }
    })
  })
}
