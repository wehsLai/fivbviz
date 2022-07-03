dataFilterUI <- function(id) {
    ns <- NS(id)
    fillCol(flex = c(NA, NA, 1),
        pickerInput(ns("round_pick"), label = "Round", choices = c(), 
                    multiple = TRUE, options = list(`actions-box` = TRUE), width = "fit", inline=TRUE),
        actionBttn(ns("filter"), label = 'Filter', style="bordered", color="primary", size="sm"),
        textOutput(ns("msg"))
    )
}

dataFilterServer <- function(id, showSeason = TRUE) {
    moduleServer(id, function(input, output, session) {
        observe({
            output$msg <- renderText(rv$marktext)
        })
        
        observe({
            updatePickerInput(session = session, inputId = "round_pick", choices = unique(rv$ds$matches$poolRoundName))
        })
        
        # filter by selected round
        observeEvent(input$filter, {
            pick <- reactive(input$round_pick)
            if(is.null(pick()) || identical(pick(), unique(rv$ds$matches$poolRoundName))) {
                rv$fds <- rv$ds
                rv$marktext <- paste0(ifelse(showSeason, paste0(rv$ds$tournament$season, " - "), ""), rv$ds$tournament$shortNameOrName)
            } else {
                rv$fds$matches <- rv$ds$matches[rv$ds$matches$poolRoundName %in% pick(),]
                rv$fds$statistics$Player <- rv$ds$statistics$Player[rv$ds$statistics$Player$noMatch %in% rv$fds$matches$no,]
                rv$fds$statistics$Team <- rv$ds$statistics$Team[rv$ds$statistics$Team$noMatch %in% rv$fds$matches$no,]
                rv$marktext <- paste0(ifelse(showSeason, paste0(rv$ds$tournament$season, " - "), ""), 
                                      rv$ds$tournament$shortNameOrName, " - ", paste0(pick(), collapse = ", "))
            }
            # update_agg_table()
        })
    })
}
