#' Loads all necessary datasets
#' Object names:  DAM, RTM, interchange, outage, weather
#'
#' @param weather Should we load the large (2GB) weather dataset?
#' @export

load_data <- function(weather = FALSE) {

  tryCatch({
    as.numeric(x)
  },
  warning = function(w) {
    cat("Warning: Re-loading data from source will overwrite the pre-loaded datasets (2024 data).")
    cont <- readline('Do you wish to continue? [Y/N] ')
    if(cont != 'Y') stop('Aborted by user')
    return(suppressWarnings(as.numeric(x)))
  }
  )

  # From OASIS; saved on personal dropbox
  DAM <- readr::read_csv("https://www.dropbox.com/scl/fi/h6lfq64dm18euxryhsdy6/DAM.csv?rlkey=ky5qvtf9syrwfxq3yqll49hwr&st=ysokq9yv&dl=1")
  RTM <- readr::read_csv("https://www.dropbox.com/scl/fi/pwti4xeusovz9m03qryfq/RTM.csv?rlkey=0bx0epm22p0znygf7o8qhmr1o&st=117jqq7n&dl=1")
  interchange <- readr::read_csv("https://www.dropbox.com/scl/fi/te73h6n3zb8hvyqcfjxlc/interchange.csv?rlkey=mkko3t38vk0rrlqswaeiizt52&st=g5wlbqkx&dl=1")
  outage <- readr::read_csv("https://www.dropbox.com/scl/fi/9z1fpk8qw1wy30hjihvc1/outageFull.csv?rlkey=b1yo71r0eqil9oyqhvoo2vcbw&st=muchqp3y&dl=1")

  # From HerbieData; saved on personal dropbox
  weather <- readr::read_csv("https://www.dropbox.com/scl/fi/qj2pfrol8uj5t2hiw9nc7/fullWeatherData.csv?rlkey=teqpy0igiq7n587dqsjmzdu09&st=o8rjd6he&dl=1")



  a <- RTM |>
    dplyr::filter(LMP_TYPE == "LMP") |>
    dplyr::group_by(OPR_DT, OPR_HR, NODE_ID) |>
    dplyr::mutate(value = mean(VALUE)) |>
    dplyr::ungroup() |>
    dplyr::distinct(OPR_DT, OPR_HR, NODE_ID, .keep_all = TRUE) |>
    dplyr::select(OPR_DT, OPR_HR, NODE_ID, value) |>
    dplyr::mutate(OPR_HR = as.character(OPR_HR))

  b <- DAM |>
    dplyr::filter(LMP_TYPE == "LMP") |>
    dplyr::select(OPR_DT, OPR_HR, NODE_ID, MW) |>
    dplyr::mutate(OPR_HR = as.character(OPR_HR))

  price_data <- b |>
    dplyr::inner_join(a, by = c("OPR_DT", "OPR_HR", "NODE_ID")) |>
    dplyr::mutate(unique_time = paste0(OPR_DT," ", OPR_HR),
           # time fails to parse with hour = 25, ignore since only 4 values
           time = lubridate::parse_date_time(unique_time, "%Y-%m-%d %H")) |>
    dplyr::filter(!is.na(time)) |>
    dplyr::select(!c(OPR_DT, OPR_HR)) |>
    tidyr::pivot_longer(cols = c(MW, value)) |>
    dplyr::mutate(name = case_when(
      name == "MW" ~ "DAM",
      name == "value" ~ "RTM"
    )) |>
    dplyr::rename("$ Price Per MW" = value) |>
    dplyr::mutate(OPR_DT = as.Date(time)) |>
    dplyr::select(!c(unique_time))


  # From EIA where two halves of 2024 are merged
  interchange <- interchange |>
    dplyr::mutate(utc_time_at_end_of_hour = parse_date_time(utc_time_at_end_of_hour,
                                                     orders = c("%m/%d/%Y %H:%M:%S %p"))) |>
    janitor::clean_names()

}


#' Loads all necessary datasets
#' Overwrites global object `weather`
#'
#' @export
load_weather_data <- function() {

  # From HerbieData; saved on personal dropbox
  weather <<- readr::read_csv("https://www.dropbox.com/scl/fi/qj2pfrol8uj5t2hiw9nc7/fullWeatherData.csv?rlkey=teqpy0igiq7n587dqsjmzdu09&st=o8rjd6he&dl=1")

}

