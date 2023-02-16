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
#' @param progress Should a progress bar be displayed? 
#' 
#' @seealso \code{\link{export_decentlab_time_series}}
#' 
#' @return Tibble. 
#' 
#' @export
read_decentlab_api_export <- function(file, df_site_ranges = NA, 
                                      df_sensing_elements_ranges = NA,
                                      variable_switch = FALSE,
                                      date_round = FALSE, progress = FALSE) {
  
  purrr::map(
    file,
    ~read_decentlab_api_export_worker(
      file = .,
      df_site_ranges = df_site_ranges,
      df_sensing_elements_ranges = df_sensing_elements_ranges,
      variable_switch = variable_switch,
      date_round = date_round
    ),
    .progress = progress
  ) %>% 
    purrr::list_rbind()
  
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
  
  # Do the join, a conditional or an inequality join
  df <- df %>% 
    left_join(
      df_ranges,
      by = join_by(
        sensor_id == sensor_id,
        between(date, date_start, date_end)
      )
    ) %>% 
    select(-date_start,
           -date_end)

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
  
  # Select variables to use in join, no variable used here
  df_ranges <- df_ranges %>% 
    select(sensor_id,
           sensing_element_id,
           date_start,
           date_end)
  
  # Do the join, a conditional or an inequality join
  df <- df %>% 
    left_join(
      df_ranges,
      by = join_by(
        sensor_id == sensor_id,
        between(date, date_start, date_end)
      )
    ) %>% 
    select(-date_start,
           -date_end)
  
  return(df)
  
}
