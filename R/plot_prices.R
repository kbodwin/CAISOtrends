#' Plot day-ahead and real-time market trends over date range
#'
#' @param start_date A date in string form "YYYY-MM-DD"
#' @param end_date A date in string form "YYYY-MM-DD"
#' @param interactive Static plot or interactive html plot?
#'
#' @import ggplot2 plotly dplyr
#'
#' @export

plot_rtm <- function(start_date = "2024-01-01",
                     end_date = "2025-01-01",
                     interactive = TRUE) {

  start_date <- lubridate::as_datetime(start_date)
  end_date <- lubridate::as_datetime(end_date)

  if (start_date > end_date) {
    stop("start_date needs to be earlier than end_date")
  }

  # price data
  x <-
    price_data |>
      filter(between(time, start_date, end_date)) |>
      ggplot() +
      geom_line(aes(x = time, y = `$ Price Per MW`, color = name)) +
      theme_minimal() +
      labs(color = "Type of Market") +
      ggtitle(paste0("Day-Ahead Market and Real-Time Market between ", start_date, " and ", end_date))

  if (interactive) { x <- ggplotly(x)}

  return(x)
}



#' Plot locational marginal prices over date range
#'
#' @param start_date A date in string form "YYYY-MM-DD"
#' @param end_date A date in string form "YYYY-MM-DD"
#' @param interactive Static plot or interactive html plot?
#'
#' @import ggplot2 plotly dplyr
#'
#' @export


plot_lmp <- function(start_date = "2024-01-01",
                     end_date = "2025-01-01",
                     interactive = TRUE) {

  start_date <- lubridate::as_datetime(start_date)
  end_date <- lubridate::as_datetime(end_date)

  if (start_date > end_date) {
    stop("start_date needs to be earlier than date 2")
  }

  # price data
  x <- price_data |>
      filter(between(time, start_date, end_date)) |>
      ggplot() +
      geom_line(aes(x = time, y = `$ Price Per MW`, color = name)) +
      facet_wrap(~NODE_ID) +
      theme_minimal() +
      labs(color = "Type of Market") +
      ggtitle("LMP")

  if (interactive) {x <- ggplotly(x)}

  return(x)
}
