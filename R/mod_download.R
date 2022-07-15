downloadUI <- function(id, label = "Download RDS", color = "primary") {
  ns <- NS(id)
  tagList(
    downloadBttn(ns("dl"), label, style = "bordered", color = color, size = "sm")
  )
}

showRkUI <- function(id, label = "showAll", color = "primary", number = 20) {
  ns <- NS(id)
  fluidRow(
    column(width = 4, prettySwitch(ns("switch"), label, status = color, fill = TRUE)),
    column(width = 6, offset = 2, numericInput(ns("number"), label = "Players", number, min = 0))
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
    } else if (type == "dashboard") {
      output$dl <- downloadHandler(
        filename = function() {
          t <- gsub(" ", "_", paste0(tolower(rv$ds$tournament$code), ".html"))
        },
        content = function(file) {
          waiter_show(html = waiting_screen)
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report_dashboard.Rmd")
          file.copy("rmd/report_dashboard.Rmd", tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params <- list(
            ds = rv$fds,
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
          waiter_hide()
        }
      )
    } else if (type == "html") {
      output$dl <- downloadHandler(
        filename = function() {
          t <- gsub(" ", "_", paste0(tolower(rv$ds$tournament$code), ".html"))
        },
        content = function(file) {
          waiter_show(html = waiting_screen)
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report_html.Rmd")
          file.copy("rmd/report_html.Rmd", tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params <- list(
            ds = rv$fds,
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
          waiter_hide()
        }
      )
    } else if (type == "p56") {
      output$dl <- downloadHandler(
        filename = function() {
          t <- gsub(" ", "_", paste0(tolower(rv$ds$tournament$code), "_p56.html"))
        },
        content = function(file) {
          waiter_show(html = waiting_screen)
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "p56.Rmd")
          file.copy("rmd/p56.Rmd", tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params <- list(
            ds = rv$fds,
            marktext = rv$marktext,
            showAll = input$switch,
            number = input$number
          )
          print(params$marktext)
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = params,
            envir = new.env(parent = globalenv())
          )
          waiter_hide()
        }
      )
    }
  })
}
