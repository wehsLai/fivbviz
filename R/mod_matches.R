matchesUI <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("table"))
}

matchesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable({
      fmt <- paste0("%0", nchar(as.character(max(rv$ds$matches$noInTournament))), "s")

      data <- rv$ds$matches %>%
        fivbvis:::v_remap(col = "status", schema = "Volley Match Status") %>%
        arrange(dateTimeUtc) %>%
        mutate(
          noInTournament = sprintf(fmt, noInTournament),
          dateTimeClient = format(dateTimeUtc, tz = Sys.timezone(), format = "%Y-%m-%d %H:%M"),
          dateClient = substring(dateTimeClient, 1, 10),
          resultText = sprintf("%s %s", matchResultText, setsResultsText) %>% stringr::str_replace_all("NA", "")
        ) %>%
        select(noInTournament, dateTimeClient, dateClient, countryName, city, hall, poolName, teamAName, teamBName, resultText, status)

      dates <- sort(unique(data$dateClient))
      teams <- sort(unique(c(data$teamAName, data$teamBName)))

      match.colDef <- list(
        noInTournament = colDef(name = "No", minWidth = 50),
        dateTimeClient = colDef(
          name = paste0("Date ", format(Sys.time(), format = "%Z")),
          minWidth = 130, filterable = TRUE,
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", "table", name),
              tags$option(value = "", "All"),
              map(dates, tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          },
          filterMethod = JS("function(rows, columnId, filterValue) {
                               return rows.filter(function(row) {
                                                    return row.values['dateClient'] == filterValue
                                                  })
                             }")
        ),
        dateClient = colDef(show = FALSE),
        countryName = colDef(
          name = "Country", minWidth = 150, filterable = TRUE,
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", "table", name),
              tags$option(value = "", "All"),
              map(sort(unique(values)), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          }
        ),
        city = colDef(
          name = "City", minWidth = 180, filterable = TRUE,
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", "table", name),
              tags$option(value = "", "All"),
              map(sort(unique(values)), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          }
        ),
        hall = colDef(
            name = "Hall", minWidth = 120, filterable = TRUE,
            filterInput = function(values, name) {
                tags$select(
                    onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", "table", name),
                    tags$option(value = "", "All"),
                    map(sort(unique(values)), tags$option),
                    "aria-label" = sprintf("Filter %s", name),
                    style = "width: 100%; height: 28px;"
                )
            }
        ),        
        poolName = colDef(
          name = "Pool", minWidth = 150, filterable = TRUE,
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", "table", name),
              tags$option(value = "", "All"),
              map(sort(unique(values)), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          }
        ),
        teamAName = colDef(
          name = "Team A", minWidth = 150, filterable = TRUE,
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", "table", name),
              tags$option(value = "", "All"),
              map(teams, tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          },
          filterMethod = JS("function(rows, columnId, filterValue) {
                               return rows.filter(function(row) {
                                                    return (row.values['teamAName'] == filterValue || row.values['teamBName'] == filterValue)
                                                  })
                             }")
        ),
        teamBName = colDef(name = "Team B", minWidth = 150),
        resultText = colDef(name = "Result", minWidth = 200),
        status = colDef(name = "Status", minWidth = 100)
      )

      reactable(
        data,
        columns = match.colDef,
        defaultColDef = df.colDef,
        style = list(fontFamily = "Source Sans Pro", minWidth = 650),
        defaultPageSize = 20,
        resizable = TRUE,
        highlight = TRUE,
        compact = TRUE,
        wrap = TRUE,
        elementId = "table"
      )
    })
  })
}
