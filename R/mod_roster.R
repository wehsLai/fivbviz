rosterUI <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("table"))
}

rosterServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable({
      ns <- NS(id)

      border <- "1px solid rgba(0, 0, 0, 0.1)"

      data <- rv$fds$players %>%
        mutate(
          isCaptain = ifelse(isCaptain == "0", "", "C"),
          nbSelTotal = nbSelWC + nbSelOG + nbSelOther,
          weight = weight / 1000000,
          height = height / 10000,
          spike = spike / 10000,
          block = block / 10000
        ) %>%
        select(isPreselected, isSelected, team.code, noShirt, isCaptain, lastNamePlayer, firstNamePlayer, teamNamePlayer, position, birthdate, weight, height, spike, block, clubName, nbSelWC, nbSelOG, nbSelOther, nbSelTotal)

      roster.colDef <- list(
        isPreselected = colDef(
          name = "Pre.", minWidth = 45, filterable = TRUE,
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == "0") "" else "\u2714\ufe0f"
          },
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
        isSelected = colDef(
          name = "Sel.", minWidth = 45, filterable = TRUE, cell = function(value) {
            # Render as an X mark or check mark
            if (value == "0") "" else "\u2714\ufe0f"
          },
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
        team.code = colDef(
          minWidth = 55, name = "Team", filterable = TRUE,
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
        noShirt = colDef(name = "Shirt No", minWidth = 55),
        isCaptain = colDef(name = "C.", minWidth = 45),
        lastNamePlayer = colDef(name = "Last Name", minWidth = 100),
        firstNamePlayer = colDef(name = "First Name", minWidth = 100),
        teamNamePlayer = colDef(name = "Shirt Name", minWidth = 100),
        position = colDef(
          name = "Pos.",
          minWidth = 55, filterable = TRUE,
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
        birthdate = colDef(name = "Birthdate"),
        weight = colDef(name = "Weight [kg]"),
        height = colDef(name = "Height [cm]"),
        spike = colDef(name = "Spike [cm]"),
        block = colDef(name = "Block [cm]"),
        clubName = colDef(name = "Club", minWidth = 180),
        nbSelWC = colDef(name = "WC"),
        nbSelOG = colDef(name = "OG"),
        nbSelOther = colDef(name = "Oth."),
        nbSelTotal = colDef(name = "Tot.")
      )

      reactable(
        data,
        columns = roster.colDef,
        defaultColDef = df.colDef,
        style = list(fontFamily = "Source Sans Pro", minWidth = 650),
        defaultPageSize = 25,
        resizable = TRUE,
        highlight = TRUE,
        compact = TRUE,
        wrap = TRUE,
        elementId = "table"
      )
    })
  })
}
