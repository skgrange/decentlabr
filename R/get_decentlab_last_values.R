#' Function to get final observational data from the Decentlab API. 
#' 
#' \code{get_decentlab_last_values} can be used to "probe" or "prime" the API to 
#' ensure the API is active and responding and explore what devices and sensors
#' a domain (or user) has access to.
#' 
#' @author Stuart K. Grange
#' 
#' @param domain A Decentlab API domain.  
#' 
#' @param key A Decentlab API key for \code{domain}. 
#' 
#' @param pause Number of seconds to pause or sleep for if the API fails to
#' respond. 
#' 
#' @param max_times Number of times to attempt to get data from the API if it 
#' fails to respond. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble. 
#' 
#' @examples 
#' 
#' # Get last values for Dencentlab's demo user
#' get_decentlab_last_values(
#'   domain = "demo.decentlab.com",
#'   key = "eyJrIjoiclhMRFFvUXFzQXpKVkZydm52b0VMRVg3M3U2b3VqQUciLCJuIjoiZGF0YS1xdWVyeS1hcGktZGVtby0yIiwiaWQiOjF9"
#' )
#' 
#' @export
get_decentlab_last_values <- function(domain, key, pause = 5, max_times = 3,
                                      verbose = FALSE) {
  
  # Set up settings for potentially multiple runs
  settings_rate <- purrr::rate_delay(pause = pause, max_times = max_times)
  
  # Wrap worker function
  get_decentlab_last_values_worker_inconsistent <- purrr::insistently(
    get_decentlab_last_values_worker, rate = settings_rate, quiet = !verbose
  )
  
  # Get last values, potentially by running the function multiple times
  df <- get_decentlab_last_values_worker_inconsistent(domain, key)
  
  return(df)
  
}


get_decentlab_last_values_worker <- function(domain, key) {
  
  getLast(domain = domain, apiKey = key) %>% 
    as_tibble() %>% 
    rename(date = time) %>% 
    mutate(
      across(c(channel, location, unit), ~if_else(. == "", NA_character_, .)),
      device =  stringr::str_split_fixed(uqk, "\\.", n = 2)[, 1]
    ) %>% 
    relocate(date,
             device,
             sensor,
             unit,
             value) %>% 
    arrange(device,
            sensor)
  
}
