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
#' @export
get_decent_lab_last_values <- function(domain, key) {
  
  getLast(domain = domain, apiKey = key) %>% 
    as_tibble() %>% 
    rename(date = time) %>% 
    mutate(across(c("channel", "location"), ~if_else(. == "", NA_character_, .)),
           device =  stringr::str_split_fixed(uqk, "\\.", n = 2)[, 1],
           device = as.integer(device)) %>% 
    relocate(date,
             device,
             sensor,
             unit,
             value)
  
}
