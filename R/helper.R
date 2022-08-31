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
        arrange(desc(Rk), desc(team.code)) %>%
        select(team.code, Attack, Block, Serve, Total, showText)
    } else {
      data <- f %>% select(x, y, team.code, showText)
    }
  } else {
    if (type == "c") {
      data <- f %>%
        filter(Rk <= 20) %>%
        arrange(desc(Rk), desc(Team)) %>%
        mutate(player.teamName = paste0(player.teamName, " - ", Team)) %>%
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

  data <- rankBySkillshow(f, type, isTeam, limit)

  # Define data and basic colDef
  if (isTeam) {
    # team colDef
    use.colDef <- list(
      Rk = colDef(minWidth = 60, headerStyle = list(borderRight = border), style = list(borderRight = border)),
      Team = colDef(minWidth = 200, headerStyle = list(borderRight = border), style = list(borderRight = border), filterable = TRUE)
    )
  } else {
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
            map(sort(unique(values)), tags$option),
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
            map(sort(unique(values)), tags$option),
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
    style = list(fontFamily = fontfamily, minWidth = 650),
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

gtstyle <- function(data, type, isTeam = FALSE, title = "", subtitle = "", spike_limit = .15, reception_limit = .20) {
  if (missing(title) || title == "") {
    switch(type,
      "c" = {
        title <- "Best Scorers"
        border <- c("Rk", "Team", "Serve")
      },
      "a" = {
        title <- "Best Attackers"
        subtitle <- paste0(subtitle, "<br>", sprintf("Load Limit: %0.f %%", spike_limit * 100))
        border <- c("Rk", "Team", "Total")
      },
      "b" = {
        title <- "Best Blockers"
        border <- c("Rk", "Team", "Total")
      },
      "s" = {
        title <- "Best Servers"
        border <- c("Rk", "Team", "Total")
      },
      "d" = {
        title <- "Best Diggers"
        border <- c("Rk", "Team", "Total")
      },
      "e" = {
        title <- "Best Setters"
        border <- c("Rk", "Team", "Total")
      },
      "r" = {
        title <- "Best Receivers"
        subtitle <- paste0(subtitle, "<br>", sprintf("Load Limit: %0.f %%", reception_limit * 100))
        border <- c("Rk", "Team", "Total")
      },
      "l" = {
        title <- "Best Liberos"
        border <- c("Rk", "Team", "Total")
      },
      "f" = {
        title <- "Team Errors"
        border <- c("Rk", "Team", "Opp. Errors")
      }
    )
  }

  out <- data %>%
    gt() %>%
    tab_header(
      title = md(sprintf("**%s**", title)),
      subtitle = md(subtitle)
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          side = c("right"),
          color = "#eee",
          weight = px(1.5)
        )
      ),
      locations = cells_body(
        columns = border
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          side = c("right"),
          color = "#eee",
          weight = px(1.5)
        )
      ),
      locations = cells_column_labels(
        columns = border
      )
    ) %>%
    tab_options(table.width = "100%")
  out
}

get_teamFlag <- function(code, source = "VW", height = 30, width = 30) {
  if (source == "VW") {
    out <- sprintf("https://images.volleyballworld.com/image/upload/f_png/t_flag/assets/flags/flag_%s", code)
  } else {
    out <- sprintf("https://www.fivb.com/~/media/flags/flag_%s.png?h=%d&w=%d", code, height, width)
  }
}

textRoster <- function(players, selected = TRUE, shortName = TRUE, numShirt = TRUE) {
  out <- ""
  if (!missing(players) && nrow(players) > 0) {
    if (selected) {
      temp <- players %>% filter(isSelected == 1)
    } else {
      temp <- players %>% filter(isPreselected == 1)
    }

    temp <- temp %>%
      mutate(
        no_text = case_when(numShirt ~ paste0("(", noShirt, ")"), TRUE ~ ""),
        player_text = case_when(
          shortName ~ paste0(trimws(teamNamePlayer), no_text),
          TRUE ~ paste0(trimws(lastNamePlayer), " ", trimws(firstNamePlayer), no_text)
        )
      ) %>%
      select(team.code, team.name, position, player_text)

    out <- map(unique(players$team.code), function(y) {
      sel_team <- temp %>% filter(team.code == y)
      if (nrow(sel_team) > 0) {
        pos_players <- map(c("S", "OP", "OH", "MB", "L", "U", "_"), function(z) {
          sel_pos <- sel_team %>% filter(position == z)
          if (nrow(sel_pos) > 0) {
            paste0(z, ": ", paste(sel_pos$player_text, collapse = ", "))
          } else {
            NA_character_
          }
        })
        paste0(y, " - ", first(sel_team$team.name), "\n", paste(pos_players[!is.na(pos_players)], collapse = "\n"))
      } else {
        NA_character_
      }
    })
    out <- paste0(out[!is.na(out)], collapse = "\n\n")
  }
  out
}

parsePeridHM <- function(period) {
  out <- as.character(period)
  out[!is.na(out)] <- sprintf("%d:%02d", hour(period[!is.na(period)]), minute(period[!is.na(period)]))
  out
}
