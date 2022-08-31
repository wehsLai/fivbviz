hline <- function(y = 0, color = "#AAA") {
  list(
    name = paste0("Value: ", y),
    type = "line",
    layer = "below",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, width = 3)
  )
}

vline <- function(x = 0, color = "#AAA") {
  list(
    name = paste0("Value: ", x),
    type = "line",
    layer = "below",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, width = 3)
  )
}

title.font <- list(
  size = 18
)

axis.font <- list(
  size = 14
)

# draw player XY chart
playerXyChart <- function(data, title = "", subtitle = "", xtitle = "Error %", ytitle = "") {
  if (missing(data)) stop("playerXyChart must input data")

  mx <- median(data$origData()$x)
  my <- median(data$origData()$y)

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
        sizeref = 2,
        sizemode = "diameter",
        opacity = .5
      )
    ) %>%
    layout(
      title = list(
        xref = "paper",
        y = 0.95,
        text = paste0("<b>", title, "</b><br><sub>", subtitle, "</sub>"),
        font = title.font
      ),
      xaxis = list(
        title = list(text = xtitle),
        autorange = "reversed",
        showgrid = TRUE,
        gridcolor = plotlyColor$grid
      ),
      yaxis = list(
        title = list(text = ytitle),
        showgrid = TRUE,
        gridcolor = plotlyColor$grid
      ),
      legend = list(itemsizing = "constant"),
      shapes = list(hline(my, plotlyColor$medianline), vline(mx, plotlyColor$medianline)),
      margin = list(
        t = 80
      ),
      font = list(family = fontfamily),
      plot_bgcolor = plotlyColor$bg,
      width = 650,
      height = 650
    )
}

# draw player Bar chart
playerBarChart <- function(data, title = "", subtitle = "", xtitle = "", ytitle = "") {
  if (missing(data)) stop("playerBarChart must input data")

  plot_ly(data, type = "bar", orientation = "h") %>%
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
      showlegend = FALSE,
      name = "Total"
    ) %>%
    layout(
      barmode = "stack",
      title = list(
        xref = "paper",
        y = 0.95,
        text = paste0("<b>", title, "</b><br><sub>", subtitle, "</sub>"),
        font = title.font
      ),
      xaxis = list(
        title = list(text = ""),
        showgrid = TRUE,
        gridcolor = plotlyColor$grid
      ),
      yaxis = list(
        title = list(text = ""),
        showgrid = FALSE,
        gridcolor = plotlyColor$grid
      ),
      legend = list(
        itemsizing = "constant",
        yanchor = "bottom",
        y = 0.02,
        xanchor = "right",
        x = 0.98
      ),
      margin = list(t = 80),
      font = list(family = fontfamily),
      plot_bgcolor = plotlyColor$bg,
      width = 650,
      height = 650
    )
}

# draw team XY chart
teamXyChart <- function(data, title = "", subtitle = "", xtitle = "Error %", ytitle = "") {
  if (missing(data)) stop("teamXyChart must input data")

  mx <- median(data$x)
  my <- median(data$y)

  # Use the ideal sizeref value
  desired_maximum_marker_size <- 10
  sizerefx <- 7 * diff(range(data$x)) / (desired_maximum_marker_size**2)
  sizerefy <- 7 * diff(range(data$y)) / (desired_maximum_marker_size**2)

  imglist <- map(c(1:nrow(data)), function(i) {
    list(
      source = get_teamFlag(data$team.code[i]),
      xref = "x",
      yref = "y",
      xanchor = "center",
      yanchor = "middle",
      x = data$x[i],
      y = data$y[i],
      sizex = sizerefx,
      sizey = sizerefy,
      opacity = .7,
      layer = "above"
    )
  })

  plot_ly(data) %>%
    add_trace(
      type = "scatter",
      mode = "text",
      x = ~x,
      y = ~y,
      text = "",
      # text = ~team.code,
      size = 30,
      hoverinfo = "x+y+text",
      hovertext = ~showText
    ) %>%
    layout(
      title = list(
        xref = "paper",
        y = 0.95,
        text = paste0("<b>", title, "</b><br><sub>", subtitle, "</sub>"),
        font = title.font
      ),
      xaxis = list(
        title = list(text = xtitle),
        autorange = "reversed",
        showgrid = TRUE,
        gridcolor = plotlyColor$grid
      ),
      yaxis = list(
        title = list(text = ytitle),
        showgrid = TRUE,
        gridcolor = plotlyColor$grid
      ),
      legend = list(itemsizing = "constant"),
      shapes = list(hline(my, plotlyColor$medianline), vline(mx, plotlyColor$medianline)),
      images = imglist,
      margin = list(
        t = 80
      ),
      font = list(family = fontfamily),
      plot_bgcolor = plotlyColor$bg,
      width = 650,
      height = 650
    )
}

# draw team Bar chart
teamBarChart <- function(data, title = "", subtitle = "", xtitle = "", ytitle = "") {
  if (missing(data)) stop("teamBarChart must input data")

  plot_ly(data, type = "bar", orientation = "h") %>%
    add_trace(
      x = ~Attack,
      y = ~team.code,
      hoverinfo = "x+y+text",
      hovertext = ~showText,
      color = "#8DA0CB",
      name = "Attack"
    ) %>%
    add_trace(
      x = ~Block,
      y = ~team.code,
      hoverinfo = "x+y+text",
      hovertext = ~showText,
      color = "#66C2A5",
      name = "Block"
    ) %>%
    add_trace(
      x = ~Serve,
      y = ~team.code,
      hoverinfo = "x+y+text",
      hovertext = ~showText,
      color = "#FC8D62",
      name = "Serve"
    ) %>%
    add_text(
      x = ~Total,
      y = ~team.code,
      text = ~Total,
      textposition = "right",
      showlegend = FALSE,
      name = "Total"
    ) %>%
    layout(
      barmode = "stack",
      title = list(
        xref = "paper",
        y = 0.95,
        text = paste0("<b>", title, "</b><br><sub>", subtitle, "</sub>"),
        font = title.font
      ),
      xaxis = list(
        title = list(text = ""),
        showgrid = TRUE,
        gridcolor = plotlyColor$grid
      ),
      yaxis = list(
        title = list(text = ""),
        showgrid = FALSE,
        gridcolor = plotlyColor$grid
      ),
      legend = list(
        itemsizing = "constant",
        yanchor = "bottom",
        y = 0.02,
        xanchor = "right",
        x = 0.98
      ),
      margin = list(
        t = 80
      ),
      font = list(family = fontfamily),
      plot_bgcolor = plotlyColor$bg,
      width = 650,
      height = 650
    )
}
