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
#' @param as_wide Should the return be in "wide" format? 
#' 
#' @param verbose Should the functions give messages? 
#' 
#' @seealso \code{\link{get_decent_lab_last_values}}
#' 
#' @return Tibble. 
#' 
#' @export
get_decent_lab_time_series <- function(domain, key, device, sensor = NA, 
                                       start = NA, as_wide = FALSE, 
                                       verbose = FALSE) {
  
  device %>% 
    purrr::map_dfr(
      ~get_decent_lab_time_series_worker(
        domain = domain,
        key = key,
        device = .,
        sensor = sensor,
        start = start,
        as_wide = as_wide,
        verbose = verbose
      )
    )
  
}


get_decent_lab_time_series_worker <- function(domain, key, device, sensor, 
                                              start, as_wide, verbose) {
  
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
  
  if (is.na(start)) {
    start <- lubridate::now() %>% 
      lubridate::floor_date("year") %>% 
      format()
  }
  
  #
  df <- tryCatch({
    query(
      domain = domain,
      apiKey = key,
      database = "main",
      doCast = FALSE,
      device = device, 
      sensor = sensor, 
      timeFilter = stringr::str_c("time >= '", start, "'"),
      timezone = "UTC"
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
