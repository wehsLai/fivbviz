tournamentsUI <- function(id) {
    ns <- NS(id)
    fillCol(height = "820", flex = c(NA, 1), 
        actionBttn(ns("open"), label = 'Open', style="bordered", color="primary", size="sm"),
        reactableOutput(ns("table"))
    )
}

tournamentsServer <- function(id, tourlist = NULL, showSeason = TRUE) {
    moduleServer(id, function(input, output, session) {
        # render tournaments table
        output$table <- renderReactable({
            reactable(
                tourlist,
                columns = tl.colDef,
                defaultColDef = df.colDef,
                defaultPageSize = 20,
                resizable = TRUE,
                filterable = TRUE,
                highlight = TRUE,
                striped = TRUE,
                compact = TRUE,
                wrap = FALSE,
                selection = "single",
                onClick = "select",
                rowStyle = JS("function(rowInfo) {
                                    if (rowInfo && rowInfo.selected) {
                                        return { backgroundColor: '#d4d4d4', boxShadow: 'inset 2px 0 0 0 #ffa62d' }
                                    }
                                }"
                )
            )
        })
        
        # selected row
        sel_row <- reactive(getReactableState("table", "selected"))
        
        # open selected tournament
        observeEvent(input$open, {
            waiter_show(html = waiting_screen)
            sel <- tournaments[sel_row(),]
            # rv$ds <- get_tournament_data(sel$no)
            rv$ds <- readRDS("data/data.rds")
            rv$fds <- rv$ds
            rv$marktext <- paste0(ifelse(showSeason, paste0(rv$ds$tournament$season, " - "), ""), rv$ds$tournament$shortNameOrName)
            waiter_hide()
        })
    })
}
