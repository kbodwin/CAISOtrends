#' Plot plant outages in date range for specific plant
#'
#' @param plant_name The name of a plant in the dataset.
#' @param start_date A date in string form "YYYY-MM-DD"
#' @param end_date A date in string form "YYYY-MM-DD"
#' @param interactive Static plot or interactive html plot?
#'
#' @examples
#' # to see available plant names:
#' unique(outage$`RESOURCE NAME`)
#'
#'
#' @import ggplot2 plotly dplyr
#'
#' @export

plot_plant_outage <- function(plant_name,
                              start_date = "2024-01-01",
                              end_date = "2025-01-01",
                              interactive = TRUE) {

  start_date <- lubridate::as_datetime(start_date)
  end_date <- lubridate::as_datetime(end_date)

  if (start_date > end_date) {
    stop("start_date needs to be earlier than end_date")
  }

  plot <- outage |>
    filter(grepl(plant_name, `RESOURCE NAME`, ignore.case = TRUE)) |>
    filter(between(as.Date(`CURTAILMENT START DATE TIME`), start_date, end_date)) |>
    mutate(Percent = (`CURTAILMENT MW` / `RESOURCE PMAX MW`) * 100) |>
    ggplot(aes(x = `CURTAILMENT START DATE TIME`,
               y = Percent,
               color = `NATURE OF WORK`)) +
    geom_segment(aes(xend = `CURTAILMENT END DATE TIME`, yend = Percent)) +
    labs(title = paste("Outages for", plant_name),
         x = "Start Date", y = "Curtailment %") +
    facet_wrap(~`OUTAGE TYPE`) +
    theme_minimal()

  if (interactive) {plot <- ggplotly(plot)}

  return(plot)
}

#' Plot weather trends in date range for specific plant
#'
#' @param plant_name The name of a plant in the dataset.
#' @param start_date A date in string form "YYYY-MM-DD"
#' @param end_date A date in string form "YYYY-MM-DD"
#' @param interactive Static plot or interactive html plot?
#'
#' @examples
#' # to see available plant names:
#' unique(outage$`RESOURCE NAME`)
#'
#'
#' @import ggplot2 plotly dplyr
#'
#' @export


plot_plant_weather <- function(plant_name,
                         start_date = "2024-01-01",
                         end_date = "2025-01-01",
                         interactive = TRUE) {

  start_date <- lubridate::as_datetime(start_date)
  end_date <- lubridate::as_datetime(end_date)

  if (start_date > end_date) {
    stop("start_date needs to be earlier than end_date")
  }

  x <- weather |>
    filter(grepl(plant_name, Plant_Name, ignore.case = TRUE)) |>
    filter(between(time, start_date, end_date)) |>
    ggplot(aes(x = time, y = `Temp (F)`)) +
    geom_line() +
    ggtitle(paste0("Temperature Data at ", plant_name)) +
    theme_minimal()

  if (interactive) {x <- ggplotly(x)}

  return(x)
}


#' Plot all plant outages in date range, by MWH and percent
#'
#' @param percent_cutoff Filter to outages above a threshold.
#' @param start_date A date in string form "YYYY-MM-DD"
#' @param end_date A date in string form "YYYY-MM-DD"
#' @param interactive Static plot or interactive html plot?
#'
#' @examples
#' # to see available plant names:
#' outage |> pull(`RESOURCE NAME`) |> unique()
#'
#'
#' @import ggplot2 plotly dplyr
#'
#' @export


plot_all_outages <- function(start_date = "2024-01-01",
                        end_date = "2025-01-01",
                        percent_cutoff = 50,
                        interactive = TRUE) {

  start_date <- lubridate::as_datetime(start_date)
  end_date <- lubridate::as_datetime(end_date)

  if (start_date > end_date) {
    stop("start_date needs to be earlier than end_date")
  }

  plot <- outage |>
    filter(between(as.Date(`CURTAILMENT START DATE TIME`), start_date, end_date)) |>
    filter(between(as.Date(`CURTAILMENT END DATE TIME`), start_date, end_date)) |>
    mutate(Percent = (`CURTAILMENT MW` / `RESOURCE PMAX MW`) * 100) |>
    filter(Percent > percent_cutoff) |>
    filter(`OUTAGE TYPE` == "FORCED") |>
    ggplot(aes(x = `CURTAILMENT START DATE TIME`,
               y = `CURTAILMENT MW`,
               color = `RESOURCE NAME`)) +
    geom_segment(aes(xend = `CURTAILMENT END DATE TIME`, yend = `CURTAILMENT MW`)) +
    labs(title = paste("Outages"),
         x = "Start Date", y = "Curtailment (MW)") +
    theme_minimal() +
    facet_wrap(~`NATURE OF WORK`) +
    theme(legend.position = "none")

  if (interactive) {plot <- ggplotly(plot)}

  return()
}


#' Plot all forced curtailments in date range, by MWH and percent
#'
#' @param start_date A date in string form "YYYY-MM-DD"
#' @param end_date A date in string form "YYYY-MM-DD"
#' @param interactive Static plot or interactive html plot?
#'
#' @import ggplot2 plotly dplyr
#'
#' @export

plot_forced_curtailments <- function(start_date = "2024-01-01",
                                    end_date = "2025-01-01",
                                    interactive = TRUE) {

  start_date <- lubridate::as_datetime(start_date)
  end_date <- lubridate::as_datetime(end_date)

  if (start_date > end_date) {
    stop("start_date needs to be earlier than end_date")
  }


  outage_avg <- outage |>
    filter(between(as.Date(`CURTAILMENT START DATE TIME`), start_date, end_date)) |>
    filter(`OUTAGE TYPE` == "FORCED") |>
    mutate(percent = `CURTAILMENT MW` / `RESOURCE PMAX MW`) |>
    group_by(`RESOURCE NAME`) |>
    dplyr::summarize(avg = mean(percent))

  plot <- outage_avg |>
    left_join(unique(outage |> select(`RESOURCE NAME`, `RESOURCE PMAX MW`)),
              by = join_by("RESOURCE NAME")) |>
    ggplot(aes(x = `RESOURCE PMAX MW`, y = avg, group = `RESOURCE NAME`)) +
    geom_point() +
    theme_minimal() +
    ggtitle("Curtailment Percentage by Resource Capacity by Power Plant")

  if (interactive) {plot <- ggplotly(plot)}

  return(plot)
}
