renTitle <- function(type) {
  out <- switch(type,
    "c" = "Best Scorers",
    "a" = "Best Attackers",
    "b" = "Best Blockers",
    "s" = "Best Servers",
    "d" = "Best Diggers",
    "e" = "Best Setters",
    "r" = "Best Receivers",
    "l" = "Best Liberos",
    "f" = "Team Errors",
  )
  out
}

renLimit <- function(type) {
  out <- if (type == "a") {
    sprintf("Load Limit: %0.f %%", spike_limit * 100)
  } else if (type == "r") {
    sprintf("Load Limit: %0.f %%", reception_limit * 100)
  } else {
    ""
  }
  out
}

renPlotly <- function(id, f, type, marktext, isTeam = FALSE, limit = 5) {
  ns <- NS(id)
  if (isTeam) {
    if (type == "c") {
      data <- f %>%
        filter(Rk <= 30) %>%
        arrange(desc(Rk), team.code) %>%
        select(team.code, Attack, Block, Serve, Total, showText)
    } else {
      data <- f %>% select(x, y, team.code, showText)
    }
  } else {
    if (type == "c") {
      data <- f %>%
        filter(Rk <= 30) %>%
        arrange(desc(Rk), Team) %>%
        select(player.teamName, Attack, Block, Serve, Total, showText)
    } else if (type == "l") {
      data <- f %>%
        select(x, y, Name, player.teamName, `Pos.`, Total, showText) %>%
        highlight_key(~Name, ns("select"))
    } else {
      data <- f %>%
        filter(`Load %` >= limit) %>%
        select(x, y, Name, player.teamName, `Pos.`, Total, showText) %>%
        highlight_key(~Name, ns("select"))
    }
  }

  switch(type,
    "c" = {
      title <- "Score Performance"
      xtitle <- ""
      ytitle <- ""
    },
    "a" = {
      title <- "Attack Performance"
      xtitle <- "Error %"
      ytitle <- "Success %"
    },
    "b" = {
      title <- "Block Performance"
      xtitle <- "Error %"
      ytitle <- "Kill Block %"
    },
    "s" = {
      title <- "Serve Performance"
      xtitle <- "Error %"
      ytitle <- "Ace %"
    },
    "d" = {
      title <- "Dig Performance"
      xtitle <- "Error %"
      ytitle <- "Dig %"
    },
    "e" = {
      title <- "Set Performance"
      xtitle <- "Error %"
      ytitle <- "Running Set %"
    },
    "r" = {
      title <- "Reception Performance"
      xtitle <- "Error %"
      ytitle <- "Excellent %"
    },
    "l" = {
      title <- "Libero Performance"
      xtitle <- "Error %"
      ytitle <- "Excellent %"
    },
    "f" = {
      title <- "Team Error"
      xtitle <- "Error Avg. by set"
      ytitle <- "Opp. Error Avg. by set"
    }
  )
  out <- if (isTeam) {
    if (type == "c") {
      # bar top 30
      teamBarChart(data, title, subtitle = marktext)
    } else {
      # txy
      teamXyChart(data, title, subtitle = marktext, xtitle = xtitle, ytitle = ytitle)
    }
  } else {
    if (type == "c") {
      # bar top 30
      playerBarChart(data, title, subtitle = marktext)
    } else {
      # pxy
      highlight(
        playerXyChart(data, title, subtitle = marktext, xtitle = xtitle, ytitle = ytitle),
        on = "plotly_selected", off = "plotly_deselect", selectize = TRUE, persistent = TRUE
      )
    }
  }
  out
}

renTable <- function(id, f, type, isTeam = FALSE, limit = 0, pageSize = 20) {
  ns <- NS(id)

  border <- "1px solid rgba(0, 0, 0, 0.1)"

  # Define data and basic colDef
  if (isTeam) {
    # team data
    if (type == "c") {
      data <- f %>%
        select(-team.code, -showText)
    } else {
      data <- f %>% select(-team.code, -y, -x, -showText)
    }
    # team colDef
    use.colDef <- list(
      Rk = colDef(minWidth = 60, headerStyle = list(borderRight = border), style = list(borderRight = border)),
      Team = colDef(minWidth = 200, headerStyle = list(borderRight = border), style = list(borderRight = border), filterable = TRUE)
    )
  } else {
    # player data
    if (type == "c") {
      data <- f %>%
        select(-player.teamName, -showText)
    } else if (type %in% c("a", "b", "s", "d", "e", "r")) {
      data <- f %>%
        filter(`Load %` >= limit) %>%
        select(-player.teamName, -y, -x, -showText)
    } else {
      data <- f %>%
        select(-player.teamName, -y, -x, -showText)
    }
    # player colDef
    use.colDef <- list(
      Rk = colDef(minWidth = 45, headerStyle = list(borderRight = border), style = list(borderRight = border)),
      No = colDef(minWidth = 45),
      Name = colDef(minWidth = 280, filterable = TRUE),
      Pos. = colDef(
        minWidth = 55, filterable = TRUE,
        filterInput = function(values, name) {
          tags$select(
            onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", ns("table"), name),
            tags$option(value = "", "All"),
            lapply(unique(values), tags$option),
            "aria-label" = sprintf("Filter %s", name),
            style = "width: 100%; height: 28px;"
          )
        }
      ),
      Team = colDef(
        minWidth = 55, headerStyle = list(borderRight = border), style = list(borderRight = border), filterable = TRUE,
        filterInput = function(values, name) {
          tags$select(
            onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", ns("table"), name),
            tags$option(value = "", "All"),
            lapply(unique(values), tags$option),
            "aria-label" = sprintf("Filter %s", name),
            style = "width: 100%; height: 28px;"
          )
        }
      )
    )
    # define player load colDef
    if (type %in% c("a", "b", "s", "d", "e", "r")) {
      use.colDef <- c(use.colDef, list(`Load %` = colDef(
        format = colFormat(digits = 2),
        filterable = TRUE,
        # Filter by minimum price
        filterMethod = JS("function(rows, columnId, filterValue) {
                               return rows.filter(function(row) {
                                                    return row.values[columnId] >= filterValue
                                                  })
                             }")
      )))
    }
  }

  # define border
  if (type %in% c("a", "b", "s", "d", "e", "r", "l")) {
    use.colDef <- c(use.colDef, list(Total = colDef(
      headerStyle = list(borderRight = border), style = list(borderRight = border),
      filterable = TRUE,
      # Filter by minimum price
      filterMethod = JS("function(rows, columnId, filterValue) {
                               return rows.filter(function(row) {
                                                    return row.values[columnId] >= filterValue
                                                  })
                             }")
    )))
  } else if (type == "c") {
    # border left
    use.colDef <- c(use.colDef, list(Total = colDef(
      headerStyle = list(borderLeft = border), style = list(borderLeft = border),
      filterable = TRUE,
      # Filter by minimum price
      filterMethod = JS("function(rows, columnId, filterValue) {
                               return rows.filter(function(row) {
                                                    return row.values[columnId] >= filterValue
                                                  })
                             }")
    )))
  } else {
    use.colDef <- c(use.colDef, list(`Opp. Errors` = colDef(headerStyle = list(borderRight = border), style = list(borderRight = border))))
  }

  # define digits
  switch(type,
    "c" = {
      use.colDef <- c(use.colDef, list(`Avg. by set` = colDef(format = colFormat(digits = 2))))
    },
    "a" = {
      use.colDef <- c(use.colDef, list(
        `Succ. %` = colDef(format = colFormat(digits = 2)),
        `Eff. %` = colDef(format = colFormat(digits = 2)),
        `Avg. by set` = colDef(format = colFormat(digits = 2))
      ))
    },
    "b" = {
      use.colDef <- c(use.colDef, list(
        `KB. %` = colDef(format = colFormat(digits = 2)),
        `Eff. %` = colDef(format = colFormat(digits = 2)),
        `Avg. by set` = colDef(format = colFormat(digits = 2))
      ))
    },
    "s" = {
      use.colDef <- c(use.colDef, list(
        `Ace %` = colDef(format = colFormat(digits = 2)),
        `Eff. %` = colDef(format = colFormat(digits = 2)),
        `Avg. by set` = colDef(format = colFormat(digits = 2))
      ))
    },
    "d" = {
      use.colDef <- c(use.colDef, list(
        `Dig %` = colDef(format = colFormat(digits = 2)),
        `Eff. %` = colDef(format = colFormat(digits = 2)),
        `Avg. by set` = colDef(format = colFormat(digits = 2))
      ))
    },
    "e" = {
      use.colDef <- c(use.colDef, list(
        `RS. %` = colDef(format = colFormat(digits = 2)),
        `Eff. %` = colDef(format = colFormat(digits = 2)),
        `Avg. by set` = colDef(format = colFormat(digits = 2))
      ))
    },
    "r" = {
      use.colDef <- c(use.colDef, list(
        `Exc. %` = colDef(format = colFormat(digits = 2)),
        `Eff. %` = colDef(format = colFormat(digits = 2)),
        `Avg. by set` = colDef(format = colFormat(digits = 2))
      ))
    },
    "l" = {
      use.colDef <- c(use.colDef, list(
        `Exc. %` = colDef(format = colFormat(digits = 2)),
        `Eff. %` = colDef(format = colFormat(digits = 2)),
        `Avg. by set` = colDef(format = colFormat(digits = 2))
      ))
    },
    "f" = {
      use.colDef <- c(use.colDef, list(
        `Avg. by set` = colDef(format = colFormat(digits = 2)),
        `Opp. Avg. by set` = colDef(format = colFormat(digits = 2))
      ))
    }
  )

  my.colDef <- colDef(
    headerVAlign = "bottom",
    minWidth = 60,
    sortNALast = TRUE
  )

  out <- reactable(
    data,
    columns = use.colDef,
    defaultColDef = my.colDef,
    style = list(fontFamily = "Source Sans Pro", fontSize = "0.875rem", minWidth = 650),
    defaultPageSize = pageSize,
    resizable = TRUE,
    highlight = TRUE,
    compact = TRUE,
    wrap = TRUE,
    defaultSorted = "Rk",
    elementId = ns("table")
  )
  out
}
