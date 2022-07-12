downloadUI <- function(id, label = "Download RDS", color = "primary") {
  ns <- NS(id)
  tagList(
    downloadBttn(ns("dl"), label, style = "bordered", color = color, size = "sm")
  )
}

downloadServer <- function(id, type) {
  moduleServer(id, function(input, output, session) {
    if (type == "ds") {
      output$dl <- downloadHandler(
        filename = function() {
          t <- paste0(tolower(rv$ds$tournament$code), ".rds")
        },
        content = function(file) {
          saveRDS(rv$ds, file = file)
        }
      )
    } else if (type == "report") {
      output$dl <- downloadHandler(
        filename = function() {
          t <- gsub(" ", "_", paste0(tolower(rv$ds$tournament$code), ".html"))
        },
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("rmd/report.Rmd", tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params <- list(
            ds = rv$ds,
            marktext = rv$marktext
          )

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = params,
            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  })
}
