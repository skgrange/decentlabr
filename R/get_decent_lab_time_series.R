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
#' @param sensor A vector of device's sensor codes.
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
#' @seealso \code{\link{get_decent_lab_last_values}}
#' 
#' @return Tibble. 
#' 
#' @export
get_decent_lab_time_series <- function(domain, key, device, sensor = NA, 
                                       start = NA, end = NA, as_wide = FALSE, 
                                       tz = "UTC", verbose = FALSE) {
  
  device %>% 
    purrr::map_dfr(
      ~get_decent_lab_time_series_worker(
        domain = domain,
        key = key,
        device = .,
        sensor = sensor,
        start = start,
        end = end,
        as_wide = as_wide,
        tz = tz,
        verbose = verbose
      )
    )
  
}


get_decent_lab_time_series_worker <- function(domain, key, device, sensor, 
                                              start, end, as_wide, tz, verbose) {
  
  # Message to user
  if (verbose) message(date_message(), "Querying device `", device, "`...")
  
  # Catch
  if (is.na(sensor[1])) {
    sensor <- "//"
  } else {
    sensor <- stringr::str_c("/", sensor, "/")
  }
  
  if (is.na(device[1])) {
    sensor <- "//"
  } else {
    device <- stringr::str_c("/", device, "/")
  }
  
  # Parse missing dates
  if (is.na(start)) {
    start <- lubridate::now() %>% 
      lubridate::floor_date("year") %>% 
      format()
  }
  
  if (is.na(end)) {
    end <- lubridate::now() %>% 
      lubridate::ceiling_date("year") %>% 
      format()
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
      sensor = sensor, 
      timeFilter = time_filter,
      timezone = tz
    ) 
  }, error = function(e) {
    tibble()
  })
  
  
  if (nrow(df) == 0L) return(tibble())
  
  df <- df %>% 
    as_tibble() %>% 
    rename(date = time) %>% 
    tidyr::separate(series, into = c("device", "sensor"), sep = "\\.") %>% 
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
