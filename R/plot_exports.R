#' Plot export trends over date range
#'
#' @param start_date A date in string form "YYYY-MM-DD"
#' @param end_date A date in string form "YYYY-MM-DD"
#' @param interactive Static plot or interactive html plot?
#'
#' @import ggplot2 plotly dplyr tidyr
#'
#' @export

plot_exports <- function(start_date = "2024-01-01",
                         end_date = "2025-01-01",
                         interactive = TRUE) {

  start_date <- lubridate::as_datetime(start_date)
  end_date <- lubridate::as_datetime(end_date)

  if (start_date > end_date) {
    stop("start_date needs to be earlier than end_date")
  }

  interchange_temp <- interchange |>
    filter(between(utc_time_at_end_of_hour, start_date, end_date))


  #Negative
  neg <- interchange_temp |>
    mutate(export = case_when(
      interchange_mw <= 0 ~ 0,
      .default = interchange_mw
    )) |>
    filter(export >= 0) |>
    mutate(day = lubridate::as_datetime(utc_time_at_end_of_hour)) |>
    group_by(day_of = lubridate::as_date(day)) |>
    dplyr::summarize(export_sum = sum(export, na.rm = TRUE)) |>
    mutate(roll_export_mean = zoo::rollmean(export_sum, k = 3, fill = NA))

  # Positive
  pos <- interchange_temp |>
    mutate(import = case_when(
      interchange_mw >= 0 ~ 0,
      .default = interchange_mw
    )) |>
    filter(import <= 0) |>
    mutate(import = import * -1) |>
    mutate(day = lubridate::as_datetime(utc_time_at_end_of_hour)) |>
    group_by(day_of = lubridate::as_date(day)) |>
    dplyr::summarize(import_sum = sum(import)) |>
    mutate(roll_import_mean = zoo::rollmean(import_sum, k = 3, fill = NA))

  p1 <- neg |>
    inner_join(pos, by = join_by(day_of)) |>
    select(day_of, roll_export_mean, roll_import_mean) |>
    mutate(`Export - Import` = roll_export_mean - roll_import_mean) |>
    pivot_longer(cols = c("roll_export_mean", "roll_import_mean", `Export - Import`)) |>
    ggplot(aes(x = day_of, y = value, color = name)) +
    geom_line() +
    theme_minimal() +
    labs(xlab = "Date",
         ylab = "MW") +
    ggtitle(paste0("Export and Import Overview between ", start_date, " and ", end_date))

  p2 <- interchange_temp |>
    mutate(`Export - Import` = interchange_mw) |>
    ggplot(aes(x = utc_time_at_end_of_hour,
               y = `Export - Import`,
               color = directly_interconnected_balancing_authority)) +
    labs(color = "Balancing Authority",
         x = "Date",
         y = "Export - Import (MW)") +
    geom_smooth(formula = y ~ x,
                method = "loess",
                se = FALSE) +
    theme_minimal() +
    ggtitle("Net Export - Import Across Balancing Authorities")

  if (interactive) {
    p1 <- ggplotly(p1)
    p2 <- ggplotly(p2)
  }

  return(list(p1, p2))
}
