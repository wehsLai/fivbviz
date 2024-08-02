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
          isCaptain = ifelse(isCaptain, "C", ""),
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
            if (value == 1) "\u2714\ufe0f" else ""
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
            if (value == 1) "\u2714\ufe0f" else ""
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
          minWidth = 50, name = "Team", filterable = TRUE,
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
        noShirt = colDef(name = "Shirt No", minWidth = 50),
        isCaptain = colDef(name = "C.", minWidth = 40),
        lastNamePlayer = colDef(name = "Last Name", minWidth = 180),
        firstNamePlayer = colDef(name = "First Name", minWidth = 180),
        teamNamePlayer = colDef(name = "Shirt Name", minWidth = 120),
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
        birthdate = colDef(name = "Birthdate", minWidth = 90),
        weight = colDef(name = "Weight\n[kg]", minWidth = 65),
        height = colDef(name = "Height\n[cm]", minWidth = 65),
        spike = colDef(name = "Spike\n[cm]", minWidth = 65),
        block = colDef(name = "Block\n[cm]", minWidth = 60),
        clubName = colDef(name = "Club", minWidth = 180),
        nbSelWC = colDef(name = "WC", maxWidth = 45),
        nbSelOG = colDef(name = "OG", maxWidth = 45),
        nbSelOther = colDef(name = "Oth.", maxWidth = 45),
        nbSelTotal = colDef(name = "Tot.", maxWidth = 45)
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
