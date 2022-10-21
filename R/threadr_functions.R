# Pulled from threadr
date_message <- function() stringr::str_c(str_date_formatted(), ": ")


str_date_formatted <- function(date = NA, time_zone = TRUE, 
                               fractional_seconds = TRUE) {
  
  # Get date if not supplied
  if (is.na(date)[1]) date <- lubridate::now(tz = Sys.timezone())
  
  # Format string
  format_date <- ifelse(
    fractional_seconds, 
    "%Y-%m-%d %H:%M:%OS3", 
    "%Y-%m-%d %H:%M:%S"
  )
  
  # Format
  x <- format(date, format = format_date, usetz = time_zone)
  
  return(x)
  
}


str_to_underscore <- function(x) {
  
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub(" |-", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_trim(x)
  return(x)
  
}


# Pulled from threadr
parse_date_arguments <- function(date, type, tz = "UTC") {
  
  # Catch for when dates are directly used as inputs
  if (lubridate::is.Date(date) || lubridate::is.POSIXt(date)) 
    date <- as.character(date)
  
  # If no date used just used system date, does not matter what type
  if (is.na(date)) {
    
    date <- lubridate::ymd(Sys.Date(), tz = tz)
    
  } else {
    
    # Get system date for future rounding
    date_system <- lubridate::ymd(Sys.Date(), tz = tz)
    
    if (type == "start") {
      
      # Catch for when years are used as dates
      if (stringr::str_count(date) == 4) date <- stringr::str_c(date, "-01-01")
      
      # Round to start of year
      date <- ifelse(
        is.na(date), 
        as.character(lubridate::floor_date(date_system, "year")),
        date
      )
      
    }
    
    if (type == "end") {
      
      # Catch for when years are used as dates
      if (stringr::str_count(date) == 4) date <- stringr::str_c(date, "-12-31")
      
      # Round to end of year
      date <- ifelse(
        is.na(date), 
        as.character(lubridate::ceiling_date(date_system, "year")), 
        date
      )
      
    }
    
    # Parse date
    date <- lubridate::parse_date_time(date, c("ymd", "dmy"), tz = tz)
    
  }
  
  return(date)
  
}


parse_unix_time <- function(x, tz = "UTC", origin = "1970-01-01") {
  as.POSIXct(x, tz = tz, origin = origin)
}


add_by_id_and_range <- function(df, test, df_map, by, min, max, add) {
  
  # TODO: make number of `by` generic. How does one dynamically construct the 
  # if_else testing statement? 
  
  # Check inputs
  stopifnot(c(test, by) %in% names(df))
  stopifnot(c(by, add, min, max) %in% names(df_map))
  stopifnot(length(by) <= 3)
  
  # Determine what NA type to use
  na_type <- df_map %>% 
    select(!!add) %>% 
    pull() %>% 
    get_na_type()
  
  # Pre-allocate variable
  df <- mutate(df, !!add := na_type)
  
  # Test and replace
  for (i in seq_len(nrow(df_map))) {
    
    # For when there is only one identifier
    if (length(by) == 1L) {
      
      # Repeatedly mutate in place
      df <- df %>%
        mutate(
          !!add := if_else(
            !!sym(by) == !!df_map[i, by, drop = TRUE] &
              !!sym(test) >= !!df_map[i, min, drop = TRUE] &
              !!sym(test) <= !!df_map[i, max, drop = TRUE],
            !!df_map[i, add, drop = TRUE],
            !!sym(add)
          )
        )
      
      # For when there are two identifiers
    } else if (length(by) == 2L) {
      
      # Repeatedly mutate in place
      df <- df %>%
        mutate(
          !!add := if_else(
            !!sym(by[1]) == !!df_map[i, by[1], drop = TRUE] &
              !!sym(by[2]) == !!df_map[i, by[2], drop = TRUE] &
              !!sym(test) >= !!df_map[i, min, drop = TRUE] &
              !!sym(test) <= !!df_map[i, max, drop = TRUE],
            !!df_map[i, add, drop = TRUE],
            !!sym(add)
          )
        )
      
      # For when there are three identifiers 
    } else if (length(by) == 3L) {
      
      # Repeatedly mutate in place
      df <- df %>%
        mutate(
          !!add := if_else(
            !!sym(by[1]) == !!df_map[i, by[1], drop = TRUE] &
              !!sym(by[2]) == !!df_map[i, by[2], drop = TRUE] &
              !!sym(by[3]) == !!df_map[i, by[3], drop = TRUE] &
              !!sym(test) >= !!df_map[i, min, drop = TRUE] &
              !!sym(test) <= !!df_map[i, max, drop = TRUE],
            !!df_map[i, add, drop = TRUE],
            !!sym(add)
          )
        )
      
    }
    
  }
  
  return(df)
  
}


get_na_type <- function(x) {
  
  if (is.logical(x)) {
    na_type <- as.logical(NA)
  } else if (is.integer(x)) {
    na_type <- as.integer(NA)
  } else if (is.double(x) && !lubridate::is.POSIXct(x)) {
    na_type <- as.numeric(NA)
  } else if (is.character(x)) {
    na_type <- as.character(NA)
  } else if (is.factor(x)) {
    na_type <- as.factor(NA)
  } else if (lubridate::is.POSIXct(x)) {
    na_type <- lubridate::NA_POSIXct_
  }
  
  return(na_type)
  
}
