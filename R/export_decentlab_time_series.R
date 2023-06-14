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
#' @param end End date to get time series for. 
#' 
#' @param as_wide Should the return be in "wide" format? The \code{as_wide} 
#' argument will not be honoured if the device's data has a \code{channel} 
#' variable because the wide format does not work well when this additional 
#' identifier is present. 
#' 
#' @param directory Directory/path where the files should be exported to. 
#' 
#' @param warn Should the function raise warnings? 
#' 
#' @param verbose Should the functions give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{get_decentlab_time_series}}
#' 
#' @return Invisible \code{domain}.
#' 
#' @export
export_decentlab_time_series <- function(domain, key, device, start, end, 
                                         as_wide = TRUE , directory, 
                                         warn = TRUE, verbose = FALSE,
                                         progress = FALSE) {
  
  # Create the directory if it does not exist
  if (!fs::dir_exists(directory)) {
    fs::dir_create(directory)
  }
  
  # Get data and export daily files into a directory
  purrr::walk(
    device,
    ~export_decentlab_time_series_worker(
      domain = domain, 
      key = key,
      device = .,
      start = start,
      end = end,
      as_wide = as_wide,
      directory = directory,
      warn = warn,
      verbose = verbose
    ),
    .progress = progress
  )
  
  return(invisible(device))
  
}


export_decentlab_time_series_worker <- function(domain, key, device, start,
                                                end, as_wide, directory, warn, 
                                                verbose) {
  
  # Get time series for sensor (called a device here)
  df <- get_decentlab_time_series(
    domain = domain,
    key = key,
    device = device,
    start = start,
    end = end,
    as_wide = as_wide,
    tz = "UTC",
    warn = warn,
    verbose = verbose
  )
  
  if (!nrow(df) == 0L) {
    
    # Split into a list
    list_df_day <- df %>% 
      mutate(day = lubridate::floor_date(date, "day")) %>% 
      dplyr::group_split(day)
    
    # Export piece by piece/day by day
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
