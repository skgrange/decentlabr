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
                                      date_round = FALSE, warn = TRUE, 
                                      progress = FALSE) {
  
  purrr::map(
    file,
    ~read_decentlab_api_export_worker(
      file = .,
      df_site_ranges = df_site_ranges,
      df_sensing_elements_ranges = df_sensing_elements_ranges,
      variable_switch = variable_switch,
      date_round = date_round,
      warn = warn
    ),
    .progress = progress
  ) %>% 
    purrr::list_rbind()
  
}


read_decentlab_api_export_worker <- function(file, df_site_ranges, 
                                             df_sensing_elements_ranges,
                                             variable_switch, date_round,
                                             warn) {
  
  # Read file
  df <- readr::read_csv(file, progress = FALSE, show_col_types = FALSE)
  
  # Try to determine sensor type from names, this will not work for sensors that
  # have not been hard coded here
  sensor_type <- dplyr::case_when(
    any(stringr::str_detect(names(df), "hpp")) ~ "senseair_hpp",
    any(stringr::str_detect(names(df), "senseair_lp8")) ~ "dl_lp8",
    any(stringr::str_detect(names(df), "vaisala_gmp343")) ~ "vaisala_gmp343",
    any(stringr::str_detect(names(df), "licor_li850")) ~ "licor_li850",
    any(stringr::str_detect(names(df), "atmos22")) ~ "dl_atm22",
    any(stringr::str_detect(names(df), "senseair_k96")) ~ "senseair_k96",
    TRUE ~ NA_character_
  )
  
  # Long data for k96 sensors
  if (is.na(sensor_type) && any(stringr::str_detect(df$sensor, "k96"))) {
    sensor_type <- "senseair_k96"
    is_long <- TRUE
  } else {
    is_long <- FALSE
  }
  
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
  
  # Make table longer, or at least rename variable to variable
  if (!is_long) {
    df <- tidyr::pivot_longer(
      df, -c(date, sensor_id, sensor_type), names_to = "variable"
    )
  } else {
    df <- rename(df, variable = sensor)
  }

  # The k96 sensors have sensors within sensors
  if (sensor_type == "senseair_k96" & is_long) {

    # Get the sensing elements within the sensor packages, usually three
    df_sensing_elements <- df %>%
      filter(variable == "senseair_k96_sensor_id") %>%
      distinct(sensor_id,
               channel,
               value) %>%
      rename(sensing_element_id = value) %>% 
      mutate(sensing_element_id = as.integer(sensing_element_id))
    
    # Replicate `nrow(df_sensing_elements)` times the common variables shared 
    # among all sensing elements
    df_common <- df %>% 
      filter(is.na(channel)) %>%
      select(-channel) %>%
      left_join(
        df_sensing_elements, by = join_by(sensor_id), relationship = "many-to-many"
      )
    
    # df_common %>% 
    #   count(sensor_id,
    #         channel,
    #         sensing_element_id,
    #         variable)
    
    # Not common variables require sensing_element_id
    df_not_common <- df %>% 
      filter(!is.na(channel)) %>% 
      left_join(df_sensing_elements, by = join_by(sensor_id, channel))
    
    # df_not_common %>% 
    #   count(sensor_id,
    #         channel,
    #         sensing_element_id,
    #         variable)
    
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
    if (warn) {
      cli::cli_alert_warning("Sensing element input has been ignored...")
    }
  }
  
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
    c("sensor_id", "sensing_element_id", "date_start", "date_end") %in% 
      names(df_ranges)
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
