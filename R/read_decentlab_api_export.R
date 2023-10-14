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
#' filtered to those contained in \code{\link{read_decentlab_variables}}.
#' 
#' @param date_round Should the dates be rounded to seconds? Sometimes, the API
#' returns data with sub-second accuracy.
#'
#' @param calculate_date Should the \code{date} variable calculated from the 
#' API's date which represents the date when the data were inserted into the
#' remote database and therefore represent the end of the measurement period 
#' with a minor delay? The date from the API is renamed to \code{date_end} 
#' (after being floor rounded) and both \code{date_round} and 
#' \code{calculate_date} cannot be \code{TRUE}. 
#' 
#' @param distinct Should only distinct sensor-variable-date combinations be 
#' returned? Duplicated dates are very rare, but can occur.
#' 
#' @param warn Should the function raise warnings? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @seealso \code{\link{export_decentlab_time_series}}, 
#' \code{\link{read_decentlab_variables}}.
#' 
#' @return Tibble. 
#' 
#' @export
read_decentlab_api_export <- function(file, df_site_ranges = NA, 
                                      df_sensing_elements_ranges = NA,
                                      variable_switch = FALSE,
                                      date_round = FALSE, calculate_date = FALSE,
                                      distinct = FALSE, warn = TRUE, 
                                      progress = FALSE) {
  
  # Check for an incomparable pair of arguments
  if (date_round & calculate_date) {
    cli::cli_abort("Both `date_round` and `calculate_date` cannot be TRUE.")
  }
  
  # Read each element in `file`
  purrr::map(
    file,
    ~read_decentlab_api_export_worker(
      file = .,
      df_site_ranges = df_site_ranges,
      df_sensing_elements_ranges = df_sensing_elements_ranges,
      variable_switch = variable_switch,
      date_round = date_round,
      calculate_date = calculate_date,
      distinct = distinct,
      warn = warn
    ),
    .progress = progress
  ) %>% 
    purrr::list_rbind()
  
}


read_decentlab_api_export_worker <- function(file, df_site_ranges, 
                                             df_sensing_elements_ranges,
                                             variable_switch, date_round, 
                                             calculate_date, distinct, warn) {
  
  # Read file
  df <- readr::read_csv(file, progress = FALSE, show_col_types = FALSE)
  
  # Is the table in long format?
  is_long <- all(c("date", "date_unix", "device", "sensor", "value") %in% names(df))
  
  # If not long, make longer or rename sensor to variable to keep the logic 
  # identical
  if (!is_long) {
    df <- tidyr::pivot_longer(
      df, -c(date, date_unix, device), names_to = "variable"
    )
  } else {
    df <- rename(df, variable = sensor)
  }
  
  # Get unique variable elements
  variable_unique <- unique(df$variable)
  
  # Determine determine sensor type, this will not work for sensors that have 
  # not been hard coded here
  sensor_type <- dplyr::case_when(
    any(stringr::str_detect(variable_unique, "hpp")) ~ "senseair_hpp",
    any(stringr::str_detect(variable_unique, "senseair_lp8|_last$")) ~ "dl_lp8",
    any(stringr::str_detect(variable_unique, "vaisala_gmp343")) ~ "vaisala_gmp343",
    any(stringr::str_detect(variable_unique, "licor_li850")) ~ "licor_li850",
    any(stringr::str_detect(variable_unique, "atmos22")) ~ "dl_atm22",
    any(stringr::str_detect(variable_unique, "k96")) ~ "senseair_k96",
    .default = "unknown"
  )
  
  # Raise a warning if sensor is unknown because some logic is based on sensor
  # type
  if (sensor_type == "unknown") {
    cli::cli_alert_warning("Sensor type cannot be determined for `{file}`...")
  }
  
  # Clean table slightly, only use unix time for dates, the dates represent
  # the date when the data were received and inserted into the database so
  # they can be delayed and represent the end of the measurement period, 
  # whatever frequency that may be
  df <- df %>% 
    rename(sensor_id = device) %>% 
    mutate(date = parse_unix_time(date_unix, tz = "UTC"),
           sensor_id = as.integer(sensor_id),
           sensor_type = !!sensor_type,
           .keep = "unused") %>% 
    relocate(date,
             sensor_id,
             sensor_type)
  
  # Round dates to nearest second/integer
  if (date_round) {
    df <- mutate(df, date = lubridate::round_date(date, "second"))
  }
  
  # The logic to calculate date start (`date`) from `date_end` which is what the 
  # dates are from the api
  if (calculate_date) {
    
    # The measurement periods of the different sensor types
    seconds_period <- dplyr::case_when(
      sensor_type %in% c(
        "senseair_hpp", "vaisala_gmp343", "licor_li850", "senseair_k96"
      ) ~
        60,
      sensor_type %in% c("dl_lp8", "dl_atm22") ~ 600
    )
    
    # Floor round date_end to the second and subtract the measurement period to
    # calculate the starting date of the aggregation period
    df <- df %>% 
      mutate(date_end = lubridate::floor_date(date, "second"),
             date = date_end - !!seconds_period)
    
  }
  
  # Remove duplicated observations, very rare
  if (distinct && sensor_type != "senseair_k96") {
    
    # Get row count before distinct call
    nrow_pre_distinct <- nrow(df)
    
    # Drop duplicated sensor-date-variable combinations
    df <- df %>% 
      distinct(date,
               sensor_id,
               variable,
               .keep_all = TRUE)
    
    # Test if duplicated dates were removed
    if (nrow(df) < nrow_pre_distinct) {
      cli::cli_alert_info(
        "{threadr::cli_date()} Duplicated dates detected, these have been removed..."
      )
    }
    
  }
  
  # The k96 sensors have sensors within sensors
  if (sensor_type == "senseair_k96") {

    # Get the sensing elements within the sensor packages, usually three
    df_sensing_elements <- df %>% 
      filter(variable == "senseair_k96_sensor_id") %>%
      distinct(sensor_id,
               channel,
               value) %>%
      rename(sensing_element_id = value) %>% 
      mutate(sensing_element_id = as.character(sensing_element_id))
    
    # There is a file that contains a duplicated channel and sensing element id
    # pair, ignore this file for now
    if (anyDuplicated(df_sensing_elements$channel) != 0L) {
      cli::cli_alert_warning(
        "`{file}` has duplicated `channel` and `sensing_element_ids`, this file has been ignored..."
      )
      return(tibble())
    }
    
    # Replicate `nrow(df_sensing_elements)` times the common variables shared 
    # among all sensing elements
    df_common <- df %>% 
      filter(is.na(channel)) %>%
      select(-channel) %>%
      left_join(
        df_sensing_elements, by = join_by(sensor_id), relationship = "many-to-many"
      )
    
    # Not common variables require sensing_element_id
    df_not_common <- df %>% 
      filter(!is.na(channel)) %>% 
      left_join(
        df_sensing_elements, 
        by = join_by(sensor_id, channel), 
        relationship = "many-to-one"
      )
    
    # Bind again and drop channel
    df <- df_common %>% 
      bind_rows(df_not_common) %>% 
      select(-channel)
    
  }
  
  # Use variable names to switch name and filter table
  if (variable_switch) {
    df <- df %>% 
      inner_join(
        read_decentlab_variables(), by = join_by(sensor_type, variable)
      ) %>% 
      select(-variable) %>% 
      rename(variable = variable_clean) %>% 
      relocate(variable,
               .after = sensor_type)
  }
  
  # Join sites if a table was passed to function
  df <- join_sites_by_date_range(df, df_site_ranges)
  
  # Join sensing element if a table was passed to function
  if (sensor_type != "senseair_k96") {
    df <- join_sensing_element_id_by_date_range(df, df_sensing_elements_ranges)
  } else {
    if (warn & is.data.frame(df_sensing_elements_ranges)) {
      cli::cli_alert_warning(
        "`df_sensing_elements_ranges` input has been ignored for a K96 sensor data file..."
      )
    }
  }
  
  # Final arranging
  df <- df %>% 
    relocate(sensor_id,
             sensing_element_id,
             sensor_type,
             site,
             date,
             dplyr::matches("date_end")) %>%
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
    c("sensor_id", "site", "date_start_range", "date_end_range") %in%
      names(df_ranges)
  )
  
  # Check if the data types are correct
  stopifnot(
    lubridate::is.POSIXt(df_ranges$date_start_range), 
    lubridate::is.POSIXt(df_ranges$date_end_range)
  )
  
  # Do the join, a conditional or an inequality join
  df <- df %>% 
    left_join(
      df_ranges,
      by = join_by(
        sensor_id == sensor_id,
        between(date, date_start_range, date_end_range)
      )
    ) %>% 
    select(-date_start_range,
           -date_end_range)

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
    c("sensor_id", "sensing_element_id", "date_start_range", "date_end_range") %in% 
      names(df_ranges)
  )
  
  # Check if the data types are correct
  stopifnot(
    lubridate::is.POSIXt(df_ranges$date_start_range), 
    lubridate::is.POSIXt(df_ranges$date_end_range)
  )
  
  # Select variables to use in join, no variable used here
  df_ranges <- df_ranges %>% 
    select(sensor_id,
           sensing_element_id,
           date_start_range,
           date_end_range)
  
  # Do the join, a conditional or an inequality join
  df <- df %>% 
    left_join(
      df_ranges,
      by = join_by(
        sensor_id == sensor_id,
        between(date, date_start_range, date_end_range)
      )
    ) %>% 
    select(-date_start_range,
           -date_end_range)
  
  return(df)
  
}
