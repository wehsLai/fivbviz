downloadUI <- function(id, label = "Download RDS") {
  ns <- NS(id)
  tagList(
    downloadBttn(ns("rds"), label, style = "bordered", color = "primary", size = "sm")
  )
}

downloadServer <- function(id, dl_all = TRUE) {
  moduleServer(id, function(input, output, session) {
    output$rds <- downloadHandler(
      filename = function() {
        t <- ifelse(dl_all, paste0(rv$ds$tournament$code, "_", "all"), paste0(rv$ds$tournament$code, "_", "filter"))
        t <- paste0(t, "_", Sys.Date(), ".rds")
      },
      content = function(file) {
        if (dl_all) {
          saveRDS(rv$ds, file = file)
        } else {
          saveRDS(rv$fds, file = file)
        }
      }
    )
  })
}
