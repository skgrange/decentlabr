#' Function to get observational data from the Decentlab API.
#' 
#' @author Stuart K. Grange
#' 
#' @param domain A Decentlab API domain.  
#' 
#' @param key A Decentlab API key for \code{domain}. 
#' 
#' @return Tibble. 
#' 
#' @examples 
#' 
#' # Get last values for Dencentlab's demo user
#' get_decent_lab_last_values(
#'   domain = "demo.decentlab.com",
#'   key = "eyJrIjoiclhMRFFvUXFzQXpKVkZydm52b0VMRVg3M3U2b3VqQUciLCJuIjoiZGF0YS1xdWVyeS1hcGktZGVtby0yIiwiaWQiOjF9"
#' )
#' 
#' @export
get_decent_lab_last_values <- function(domain, key) {
  
  getLast(domain = domain, apiKey = key) %>% 
    as_tibble() %>% 
    rename(date = time) %>% 
    mutate(
      across(c("channel", "location", "unit"), ~if_else(. == "", NA_character_, .)),
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
