#' Function to get observational data from the Decentlab API.
#' 
#' @author Stuart K. Grange
#' 
#' @param domain A Decentlab API domain.  
#' 
#' @param key A Decentlab API key for \code{domain}. 
#' 
#' @param device A vector of device codes.
#' 
#' @param start Start date to get time series for. 
#' 
#' @param end End date to get time series for
#' 
#' @param as_wide Should the return be in "wide" format? 
#' 
#' @param tz Time-zone for the time series to be returned in. 
#' 
#' @param verbose Should the functions give messages? 
#' 
#' @seealso \code{\link{get_decentlab_last_values}}
#' 
#' @return Tibble. 
#' 
#' @examples 
#' 
#' # Get a device's observations for a short time period
#' get_decentlab_time_series(
#'   domain = "demo.decentlab.com",
#'   key = "eyJrIjoiclhMRFFvUXFzQXpKVkZydm52b0VMRVg3M3U2b3VqQUciLCJuIjoiZGF0YS1xdWVyeS1hcGktZGVtby0yIiwiaWQiOjF9",
#'   device = 3001,
#'   start = lubridate::today() - lubridate::days(1),
#'   end = lubridate::today() + lubridate::days(1)
#' )
#' 
#' # Get a device's observations for a short time period in wide format
#' get_decentlab_time_series(
#'   domain = "demo.decentlab.com",
#'   key = "eyJrIjoiclhMRFFvUXFzQXpKVkZydm52b0VMRVg3M3U2b3VqQUciLCJuIjoiZGF0YS1xdWVyeS1hcGktZGVtby0yIiwiaWQiOjF9",
#'   device = 3001,
#'   start = lubridate::today() - lubridate::days(1),
#'   end = lubridate::today() + lubridate::days(1),
#'   as_wide = TRUE
#' )
#' 
#' @export
get_decentlab_time_series <- function(domain, key, device, start = NA, end = NA, 
                                      as_wide = FALSE, tz = "UTC", 
                                      verbose = FALSE) {
  
  device %>% 
    purrr::map_dfr(
      ~get_decentlab_time_series_worker(
        domain = domain,
        key = key,
        device = .,
        start = start,
        end = end,
        as_wide = as_wide,
        tz = tz,
        verbose = verbose
      )
    )
  
}


get_decentlab_time_series_worker <- function(domain, key, device, start, end, 
                                             as_wide, tz, verbose) {
  
  # Message to user
  if (verbose) message(date_message(), "Getting data for device `", device, "`...")
  
  # Parse some of the arguments that are fed to the API
  if (is.na(device[1])) {
    sensor <- "//"
  } else {
    device <- stringr::str_c("/", device, "/")
  }
  
  # Parse missing dates
  start <- parse_date_arguments(start, type = "start") %>% 
    str_date_formatted(time_zone = FALSE, fractional_seconds = FALSE)
  
  # If end is missing, push to the last instant of the day
  if (is.na(end[1])) {
    end <- (lubridate::today() + lubridate::days(1)) - lubridate::seconds(1)
    end <- as.character(end)
  } else {
    end <- parse_date_arguments(end, type = "end") %>% 
      str_date_formatted(time_zone = FALSE, fractional_seconds = FALSE)
  }
  
  # Build time filter string
  time_filter <- glue::glue(
    "time >= '", start, "' AND time <= '", end, "'"
  ) %>% 
    as.character()
  
  # Query API
  # TODO: Handle errors better
  df <- tryCatch({
    query(
      domain = domain,
      apiKey = key,
      database = "main",
      doCast = FALSE,
      device = device, 
      sensor = "//", 
      timeFilter = time_filter,
      timezone = tz
    )
  }, error = function(e) {
    tibble()
  })
  
  # If no data, return here
  if (nrow(df) == 0L) return(tibble())
  
  # When data are available
  df <- df %>% 
    as_tibble() %>% 
    rename(date = time) %>% 
    tidyr::separate(
      series, 
      into = c("device", "sensor"), 
      sep = "\\.", 
      extra = "merge"
    ) %>% 
    mutate(date_unix = as.numeric(date),
           device = as.integer(device),
           sensor = str_to_underscore(sensor)) %>% 
    relocate(date,
             date_unix)
  
  # Reshape to wide table if needed
  if (as_wide) {
    df <- tidyr::pivot_wider(df, names_from = sensor)
  }
  
  return(df)
  
}


# # Separate the variables format a few things
# df <- df %>%
#   as_tibble() %>%
#   rename(date = time) %>%
#   tidyr::separate(
#     series,
#     into = c("device", "sensor", "channel"),
#     sep = "\\.",
#     extra = "merge",
#     fill = "right"
#   ) %>%
#   mutate(date_unix = as.numeric(date),
#          device = as.integer(device),
#          sensor = str_to_underscore(sensor)) %>%
#   relocate(date,
#            date_unix) %>%
#   drop_na_columns()
# 
# # Does the table have a channel variable
# has_channel <- "channel" %in% names(df)
# 
# # Make channel variable an integer if it exists
# if (has_channel) {
#   df <- df %>% 
#     mutate(channel = stringr::str_remove(channel, "^ch"),
#            channel = as.integer(channel))
# }
# 
# # Reshape to wide table if desired, but only can be done when no channel
# # variable is present
# if (as_wide) {
#   if (!has_channel) {
#     # Do the reshaping
#     df <- tidyr::pivot_wider(df, names_from = sensor)
#   } else {
#    warning(
#      "The observations have a `channel` variable so the `as_wide` argument cannot be honored."
#     )
#   }
# }


# # Pulled from threadr
# drop_na_columns <- function(df) {
#   index <- colSums(is.na(df)) < nrow(df)
#   df <- df[, index, drop = FALSE]
#   return(df)
# }
