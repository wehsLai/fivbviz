tournamentsUI <- function(id) {
  ns <- NS(id)
  fillCol(
    height = "790", flex = c(NA, 1),
    actionBttn(ns("open"), label = "Open", style = "bordered", color = "primary", size = "sm"),
    reactableOutput(ns("table"))
  )
}

tournamentsServer <- function(id, showSeason = TRUE) {
  moduleServer(id, function(input, output, session) {
    # get tournaments list
    tournaments <- reactive({
      pl <- list(Fields = "No Season ShortNameOrName StartDate EndDate Gender OrganizerType Status")
      # readRDS("data/tournaments.rds")
      v_get_volley_tournament_list(parent = pl) %>% arrange(desc(startDate))
    })

    # render tournaments table
    output$table <- renderReactable({
      tl.colDef <- list(
        no = colDef(minWidth = 50, align = "left"),
        shortNameOrName = colDef(name = "Tournament", minWidth = 280)
      )

      reactable(
        tournaments(),
        columns = tl.colDef,
        defaultColDef = df.colDef,
        defaultPageSize = 15,
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

    cal_fds <- reactive({
      agg <- get_agg(rv$fds$statistics)
      rv$fds$statistics$team_agg <- agg$team_agg
      rv$fds$statistics$player_agg <- agg$player_agg
    })

    # open selected tournament
    observeEvent(input$open, {
      waiter_show(html = waiting_screen)
      sel <- tournaments()[sel_row(), ]
      rv$ds <- get_tournament_data(sel$no)
      # rv$ds <- readRDS("data/data.rds")
      rv$marktext <- paste0(ifelse(showSeason, paste0(rv$ds$tournament$season, " - "), ""), rv$ds$tournament$shortNameOrName)
      rv$fds <- rv$ds
      cal_fds()
      waiter_hide()
    })
  })
}
