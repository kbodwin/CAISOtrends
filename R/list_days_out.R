
#' Get a list of outage details by plant
#'
#' @param start_date A date in string form "YYYY-MM-DD"
#' @param end_date A date in string form "YYYY-MM-DD"
#' @param reason A specific outage reason given.
#' @param percent_min A threshold; don't show outages below this threshold.
#' @param mw_min A threshold; don't show outages below this threshold.
#' @param num Number of outages to show.
#'
#' @examples
#' # To see possible outage reasons:
#' unique(outage$`NATURE OF WORK`)
#'
#'
#' @import ggplot2 plotly dplyr
#'
#' @export
list_days_out <- function(start_date = "2024-01-01 00:00:00",
                          end_date = "2024-12-31 23:59:59",
                          reason = NA,
                          percent_min = 0,
                          mw_min = 0,
                          num = 50) {

  if (stringr::str_detect(start_date, ":")) {
    start_date <- lubridate::as_datetime(start_date)
  } else {
    start_date <- lubridate::as_datetime(paste(start_date, "00:00:00"))
  }

  if (stringr::str_detect(end_date, ":")) {
    end_date <- lubridate::as_datetime(end_date)
  } else {
    end_date <- lubridate::as_datetime(paste(end_date, "23:59:59"))
  }


  reason_list <- unique(outage$`NATURE OF WORK`)
  if (start_date > end_date) {
    stop("start_date needs to be earlier than end_date")
  }
  if (!reason %in% reason_list & !is.na(reason)) {
    stop("reason not cited in outages")
  }


  disjoint_sum <- function(start, end, points, pmax) {
    breaks <- sort(unique(c(start, end)))
    `CURTAILMENT START DATE TIME` <- head(breaks, -1)
    `CURTAILMENT END DATE TIME` <- tail(breaks, -1)


    tibble(`CURTAILMENT START DATE TIME`, `CURTAILMENT END DATE TIME`) %>%
      rowwise() %>%
      mutate(`CURTAILMENT MW` = sum(points[start < `CURTAILMENT END DATE TIME` & end > `CURTAILMENT START DATE TIME`]),
             `CURTAILMENT MW` = min(`CURTAILMENT MW`, pmax)) %>%
      ungroup()

  }

  if (is.na(reason)) {
    outage_clean <- outage |>
      distinct() |>
      mutate(percent = `CURTAILMENT MW` / `RESOURCE PMAX MW`,
             `CURTAILMENT START DATE TIME` = lubridate::as_datetime(`CURTAILMENT START DATE TIME`),
             `CURTAILMENT END DATE TIME` = lubridate::as_datetime(`CURTAILMENT END DATE TIME`),
             `CURTAILMENT END DATE TIME` = if_else(`CURTAILMENT END DATE TIME` > lubridate::as_datetime(end_date), lubridate::as_datetime(end_date), `CURTAILMENT END DATE TIME`)) |>

      filter(between(as.Date(`CURTAILMENT START DATE TIME`), lubridate::as_datetime(start_date), lubridate::as_datetime(end_date)) &
               `CURTAILMENT MW` >= mw_min &
               percent >= percent_min &
               `OUTAGE TYPE` == "FORCED" )
  } else {

    outage_clean <- outage |>
      distinct() |>
      mutate(percent = `CURTAILMENT MW` / `RESOURCE PMAX MW`,
             `CURTAILMENT START DATE TIME` = lubridate::as_datetime(`CURTAILMENT START DATE TIME`),
             `CURTAILMENT END DATE TIME` = lubridate::as_datetime(`CURTAILMENT END DATE TIME`),
             `CURTAILMENT END DATE TIME` = if_else(`CURTAILMENT END DATE TIME` > lubridate::as_datetime(end_date), lubridate::as_datetime(end_date), `CURTAILMENT END DATE TIME`)) |>

      filter(between(as.Date(`CURTAILMENT START DATE TIME`), lubridate::as_datetime(start_date), lubridate::as_datetime(end_date)) &
               `CURTAILMENT MW` >= mw_min &
               percent >= percent_min &
               `OUTAGE TYPE` == "FORCED" &
               `NATURE OF WORK` == reason)

  }

  outage_resource_max <- outage_clean |>
    group_by(`RESOURCE NAME`) |>
    summarize(`RESOURCE PMAX MW` = max(`RESOURCE PMAX MW`)) |>
    ungroup()


  result <- outage_clean %>%
    group_by(`RESOURCE NAME`) %>%
    group_modify(~ {
      pmax <- max(.x$`RESOURCE PMAX MW`, na.rm = TRUE)
      disjoint_sum(.x$`CURTAILMENT START DATE TIME`, .x$`CURTAILMENT END DATE TIME`, .x$`CURTAILMENT MW`, pmax)
    }) %>%
    ungroup() %>%
    inner_join(outage_resource_max, by = "RESOURCE NAME") %>%
    mutate(time_out = difftime(`CURTAILMENT END DATE TIME`, `CURTAILMENT START DATE TIME`, units = "days")) %>%
    filter( `CURTAILMENT MW` >= mw_min) |>
    group_by(`RESOURCE NAME`) %>%
    mutate(prop_time_out = as.numeric(time_out)/sum(as.numeric(time_out))) %>%
    summarize(time_out = round(sum(time_out), 2),
              avg_curtailment = sum(`CURTAILMENT MW` * prop_time_out),
              max_capacity = first(`RESOURCE PMAX MW`)) %>%
    arrange(desc(time_out)) %>%
    slice_max(time_out, n = num)

  return(result)

}
