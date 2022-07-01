tournamentsUI <- function(id) {
    ns <- NS(id)
    fillCol(flex = c(NA,1),
        fillRow(flex = c(NA,1,NA),
            actionBttn(ns("open"), label = 'Open', style="bordered", color="primary"),
            textOutput(ns("msg")),
            downloadBttn(ns('dl_rds'), 'Download RDS', style="bordered", color="primary")),
        reactableOutput(ns("table"))
    )
}

tournamentsServer <- function(id, tourlist = NULL) {
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
        selected <- reactive(getReactableState("table", "selected"))
        rv <- reactiveValues(data = NULL)
        # open selected tournament
        observeEvent(input$open, {
            waiter_show(html = waiting_screen)
            sel <- tournaments[selected(),]
            # rv$data <- get_tournament_data(sel$no)
            rv$data <- readRDS("data/data.rds")
            output$msg <- renderText(paste0("  Using Dataset: ", sel$season, " - ", sel$shortNameOrName))
            waiter_hide()
        })
        
        output$dl_rds <- downloadHandler(
            filename = function() {
                "data.rds"
            },
            content = function(file) {
                saveRDS(rv$data, file = file)
            }
        )
        return(rv)
    })
}
