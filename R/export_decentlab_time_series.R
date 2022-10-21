#' Function to get data from the Decentlab API and export daily files to a 
#' directory. 
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
#' @param directory Directory where the files should be exported to. 
#' 
#' @param verbose Should the functions give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{get_decentlab_time_series}}
#' 
#' @return Invisible \code{domain}.
#' 
#' @export
export_decentlab_time_series <- function(domain, key, device, start, end,
                                          directory, verbose = FALSE) {
  
  # Get data and export daily files into a directory
  device %>% 
    purrr::walk(
      ~export_decentlab_time_series_worker(
        domain = domain, 
        key = key,
        device = .,
        start = start,
        end = end,
        directory = directory,
        verbose = verbose
      )
    )
  
  return(invisible(device))
  
}


export_decentlab_time_series_worker <- function(domain, key, device, start,
                                                 end, directory, verbose) {
  
  # Get time series for sensor (called a device here)
  df <- get_decentlab_time_series(
    domain = domain,
    key = key,
    device = device,
    start = start,
    end = end,
    as_wide = TRUE,
    tz = "UTC",
    verbose = verbose
  )
  
  if (!nrow(df) == 0L) {
    
    # Split into a list
    list_df_day <- df %>% 
      mutate(day = lubridate::floor_date(date, "day")) %>% 
      dplyr::group_split(day)
    
    # Export piece by piece
    purrr::walk(
      list_df_day, export_decentlab_time_series_file_writter, directory = directory
    )
    
  }
  
  return(invisible(df))
  
}


export_decentlab_time_series_file_writter <- function(df, directory) {
  
  # Build a file name
  file_name <- stringr::str_c(
    format(df$day[1]), "_device_", df$device[1], "_decentlab_api_data.csv.bz2"
  )
  
  # Build a path
  file_name <- fs::path(directory, file_name)
  
  # Export
  df %>% 
    select(-day) %>% 
    mutate(date = format(date)) %>% 
    readr::write_csv(file_name)
  
  return(invisible(file_name))
  
}