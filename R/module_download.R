downloadUI <- function(id) {
    ns <- NS(id)
    tagList(
        downloadBttn(ns('dl_rds'), 'Download RDS', style="bordered", color="primary")
    )
}

downloadServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        output$dl_rds <- downloadHandler(
            filename = function() {
                "data.rds"
            },
            content = function(file) {
                saveRDS(data, file = file)
            }
        )       
    })
}