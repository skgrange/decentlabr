#' Function to read exported Decentlab API files with options to add site and 
#' sensing element IDs. 
#' 
#' @author Stuart K. Grange.
#' 
#' @param file Vector of file names of files written by 
#' \code{\link{export_decentlab_time_series}}. 
#' 
#' @param df_site_ranges An optional data frame containing site information. 
#' 
#' @param df_sensing_elements_ranges An optional data frame containing sensing
#' element information. 
#' 
#' @param variable_switch Should the variables be switched to "clean" names and
#' filtered to those contained in \code{\link{decentlab_variables_look_up}}.
#' 
#' @param date_round Should the dates be rounded to seconds? Sometimes, the API
#' returns data with sub-second accuracy. 
#' 
#' @seealso \code{\link{export_decentlab_time_series}}
#' 
#' @return Tibble. 
#' 
#' @export
read_decentlab_api_export <- function(file, df_site_ranges = NA, 
                                      df_sensing_elements_ranges = NA,
                                      variable_switch = FALSE,
                                      date_round = FALSE) {
  
  purrr::map_dfr(
    file,
    ~read_decentlab_api_export_worker(
      file = .,
      df_site_ranges = df_site_ranges,
      df_sensing_elements_ranges = df_sensing_elements_ranges,
      variable_switch = variable_switch,
      date_round = date_round
    )
  )
  
}


read_decentlab_api_export_worker <- function(file, df_site_ranges, 
                                             df_sensing_elements_ranges,
                                             variable_switch, date_round) {
  
  # Read file
  df <- readr::read_csv(file, progress = FALSE, show_col_types = FALSE)
  
  # Try to determine sensor type from names, this will not work for sensors that
  # have not been hard coded here
  sensor_type <- dplyr::case_when(
    any(stringr::str_detect(names(df), "hpp")) ~ "hpp",
    any(stringr::str_detect(names(df), "senseair_lp8")) ~ "dl_lp8",
    any(stringr::str_detect(names(df), "vaisala_gmp343")) ~ "vaisala",
    any(stringr::str_detect(names(df), "licor_li850")) ~ "licor",
    any(stringr::str_detect(names(df), "atmos22")) ~ "dl_atm22",
    any(stringr::str_detect(names(df), "senseair_k96")) ~ "senseair_k96",
    TRUE ~ NA_character_
  )
  
  # Clean table slightly
  df <- df %>% 
    rename(sensor_id = device) %>% 
    mutate(date = parse_unix_time(date_unix, tz = "UTC"),
           sensor_type = !!sensor_type,
           .keep = "unused") %>% 
    relocate(date,
             sensor_id,
             sensor_type)
  
  # Round dates to integers
  if (date_round) {
    df <- mutate(df, date = lubridate::round_date(date, "second"))
  }
  
  # Make table longer
  df <- tidyr::pivot_longer(
    df, -c(date, sensor_id, sensor_type), names_to = "variable"
  )
  
  # Use variable names to switch name and filter table
  if (variable_switch) {
    df <- df %>% 
      inner_join(decentlab_variables_look_up(), by = c("sensor_type", "variable")) %>% 
      select(-variable) %>% 
      rename(variable = variable_clean) %>% 
      relocate(variable,
               .after = sensor_type)
  }
  
  # Join sites if a table was passed to function
  df <- join_sites_by_date_range(df, df_site_ranges)
  
  # Join sensing element if a table was passed to function
  df <- join_sensing_element_id_by_date_range(df, df_sensing_elements_ranges)
  
  # Final arranging
  df <- df %>% 
    relocate(sensor_id,
             sensing_element_id,
             sensor_type,
             site,
             date) %>%
    arrange(sensor_id,
            variable,
            date)
  
  return(df)
  
}


join_sites_by_date_range <- function(df, df_ranges) {
  
  # If NA is passed
  if (!is.data.frame(df_ranges)) {
    df <- mutate(df, site = NA_character_)
    return(df)
  }
  
  # Check ranges input
  # Check if the needed variables exist
  stopifnot(
    c("sensor_id", "site", "date_start", "date_end") %in% names(df_ranges)
  )
  
  # Check if the data types are correct
  stopifnot(
    lubridate::is.POSIXt(df_ranges$date_start), 
    lubridate::is.POSIXt(df_ranges$date_end)
  )
  
  # Filter ranges table
  df_ranges <- inner_join(df_ranges, distinct(df, sensor_id), by = "sensor_id") %>% 
    tibble::rowid_to_column("rowid")
  
  # Join site by date range, extra join is for when there are extra variables
  # in df_ranges
  df_sites <- df %>% 
    distinct(date,
             sensor_id) %>% 
    add_by_id_and_range(
      test = "date",
      df_map = df_ranges,
      by = "sensor_id",
      min = "date_start",
      max = "date_end",
      add = "rowid"
    ) %>% 
    left_join(
      select(df_ranges, -sensor_id, -date_start, -date_end), by = "rowid"
    ) %>% 
    select(-rowid)
  
  # Join to table again
  df <- left_join(df, df_sites, by = c("date", "sensor_id"))
  
  return(df)
  
}


join_sensing_element_id_by_date_range <- function(df, df_ranges) {
  
  # If NA is passed
  if (!is.data.frame(df_ranges)) {
    df <- mutate(df, sensing_element_id = NA_character_)
    return(df)
  }
  
  # Check ranges input
  # Check if the needed variables exist
  stopifnot(
    c("sensor_id", "sensing_element_id", "date_start", "date_end") %in% names(df_ranges)
  )
  
  # Check if the data types are correct
  stopifnot(
    lubridate::is.POSIXt(df_ranges$date_start), 
    lubridate::is.POSIXt(df_ranges$date_end)
  )
  
  # Filter ranges table
  df_ranges <- inner_join(df_ranges, distinct(df, sensor_id), by = "sensor_id")
  
  df_sensing_elements <- df %>% 
    distinct(date,
             sensor_id) %>% 
    add_by_id_and_range(
      test = "date",
      df_map = df_ranges,
      by = "sensor_id",
      min = "date_start",
      max = "date_end",
      add = "sensing_element_id"
    )
  
  # Join to table again
  df <- left_join(df, df_sensing_elements, by = c("date", "sensor_id"))
  
  return(df)
  
}
