clipboardP3UI <- function(id) {
  ns <- NS(id)
  fillCol(
    flex = c(NA, NA, 1),
    tagList(
      prettyCheckbox(ns("official"), label = "Official", value = TRUE, status = "primary", icon = icon("check"), inline = T),
      prettyCheckbox(ns("latin1"), label = "latin1", value = TRUE, status = "primary", icon = icon("check"), inline = T),
      selectizeInput(ns("match_pick"), label = NULL, choices = c(), width = "100%"),
      actionButton(ns("copy_match"), label = "Copy Match", icon = fontawesome::fa_i(name = "copy"), `data-clipboard-target` = sprintf("#%s", ns("match_name"))),
      actionButton(ns("copy_p3"), label = "Copy P3", icon = fontawesome::fa_i(name = "copy"), `data-clipboard-target` = sprintf("#%s", ns("board"))),
      # trigger clipboard.js, place it at the end of the column
      uiOutput(ns("clip"), style = "visibility: hidden")
    ),
    verbatimTextOutput(ns("match_name"), placeholder = TRUE),
    verbatimTextOutput(ns("board"), placeholder = TRUE)
  )
}

clipboardP3Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    cho <- reactive({
      if (input$official) {
        out <- rv$ds$matches %>% filter(status >= 25 & resultType == 0)
      } else {
        out <- rv$ds$matches %>% filter(status >= 24 & resultType == 0)
      }

      out <- out %>%
        arrange(dateTimeUtc) %>%
        mutate(
          dateClient = format(dateTimeUtc, tz = Sys.timezone(), format = "%Y-%m-%d"),
          timeClient = format(dateTimeUtc, tz = Sys.timezone(), format = "%H:%M"),
          resultText = sprintf("%s %s", stringr::str_replace_na(matchResultText, ""), stringr::str_replace_na(setsResultsText, "")),
          showText = sprintf("%s %s %s-%s %s", noInTournament, poolName, teamACode, teamBCode, resultText)
        ) %>%
        select(no, showText, dateClient) %>%
        group_by(dateClient)

      out
    })

    observe({
      x <- cho() %>% group_map(~ setNames(.x$no, .x$showText))
      y <- cho() %>% group_keys()
      cholist <- c(NULL, setNames(x, y$dateClient))
      updateSelectizeInput(session = session, inputId = "match_pick", choices = cholist)
    })

    clipVal <- reactive({
      get_p3(rv$tournament$name, rv$ds$match, rv$ds$statistics, input$match_pick, type = "text")
    })

    output$match_name <- renderText({
      out <- cho() %>%
        ungroup() %>%
        filter(no == input$match_pick)
      out$showText
    })

    output$board <- renderText({
      # convert to latin1
      if (input$latin1) {
        iconv(clipVal(), "UTF8", "latin1")
      } else {
        clipVal()
      }
    })

    output$clip <- renderUI({
      rclipButton(
        inputId = "clipN",
        label = NULL,
        clipText = ""
      )
    })
  })
}
