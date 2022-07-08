hline <- function(y = 0, color = "#aaa") {
  list(
    name = paste0("Value: ", y),
    type = "line",
    layer = "below",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, opacity = .5)
  )
}

vline <- function(x = 0, color = "#aaa") {
  list(
    name = paste0("Value: ", x),
    type = "line",
    layer = "below",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, opacity = .5)
  )
}

# draw player XY chart
playerXyChart <- function(data, title = "", subtitle = "", xtitle = "Error %", ytitle = "") {
  if (missing(data)) stop("playerXyChart must input data")

  mx <- median(data$x)
  my <- median(data$y)

  plot_ly(data) %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~x,
      y = ~y,
      size = ~Total,
      color = ~`Pos.`,
      colors = pos.color,
      text = ~player.teamName,
      hoverinfo = "x+y+text",
      hovertext = ~showText,
      marker = list(
        sizeref = 1.8,
        sizemode = "diameter",
        opacity = .5
      )
    ) %>%
    layout(
      title = list(
        y = 0.95,
        text = paste0(
          "<b>",
          title,
          "</b><br><sub>",
          subtitle,
          "</sub>"
        )
      ),
      xaxis = list(
        title = list(text = xtitle),
        autorange = "reversed",
        showgrid = TRUE,
        gridcolor = grid.color
      ),
      yaxis = list(
        title = list(text = ytitle),
        showgrid = TRUE,
        gridcolor = grid.color
      ),
      shapes = list(hline(my, median.color), vline(mx, median.color)),
      margin = list(
        t = 80
      ), width = 650, height = 650
    )
}

# draw player Bar chart
playerBarChart <- function(data, title = "", subtitle = "", xtitle = "", ytitle = "") {
    if (missing(data)) stop("playerBarChart must input data")

    plot_ly(data, type = "bar", orientation = 'h') %>%
        add_trace(
            x = ~Attack,
            y = ~player.teamName,
            hoverinfo = "x+y+text",
            hovertext = ~showText,
            color = "#8DA0CB",
            name = "Attack"
        ) %>%
        add_trace(
            x = ~Block,
            y = ~player.teamName,
            hoverinfo = "x+y+text",
            hovertext = ~showText,
            color = "#66C2A5",
            name = "Block"
        ) %>% 
        add_trace(
            x = ~Serve,
            y = ~player.teamName,
            hoverinfo = "x+y+text",
            hovertext = ~showText,
            color = "#FC8D62",
            name = "Serve"
        ) %>%
        add_text(
            x = ~Total,
            y = ~player.teamName,
            text = ~Total,
            textposition = "right",
            showlegend = F,
            name = "Total"
        ) %>%
        layout(
            barmode = 'stack',
            title = list(
                y = 0.95,
                text = paste0(
                    "<b>",
                    title,
                    "</b><br><sub>",
                    subtitle,
                    "</sub>"
                )
            ),
            xaxis = list(
                title = list(text = ""),
                showgrid = TRUE,
                gridcolor = grid.color
            ),
            yaxis = list(
                title = list(text = ""),
                showgrid = FALSE,
                gridcolor = grid.color
            ),
            margin = list(
                t = 80
            ), width = 650, height = 650
        )
}

# draw team comparison chart (txy)
teamXyChart <- function(df, title, subtitle) {
  if (missing(df)) stop("teamXyChart must input data")

  mx <- median(df$x)
  my <- median(df$y)

  plot_ly(data) %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~x,
      y = ~y,
      size = ~Total,
      text = ~teamName,
      hoverinfo = "x+y+text",
      hovertext = ~showText,
      marker = list(
        sizeref = 1.8,
        sizemode = "diameter",
        opacity = .5
      )
    ) %>%
    layout(
      title = list(
        y = 0.95,
        text = paste0(
          "<b>",
          title,
          "</b><br><sub>",
          subtitle,
          "</sub>"
        )
      ),
      xaxis = list(
        title = list(text = "Err. %"),
        autorange = "reversed",
        showgrid = TRUE,
        gridcolor = grid.color
      ),
      yaxis = list(
        title = list(text = "Succ. %"),
        showgrid = TRUE,
        gridcolor = grid.color
      ),
      shapes = list(hline(my, median.color), vline(mx, median.color)),
      margin = list(
        t = 70
      )
    )
}
