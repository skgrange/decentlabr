#' Function to list and get information from a collection of exported Decentlab 
#' API files. 
#' 
#' \code{list_decentlab_api_files} will extract information from file names to
#' allow for easy filtering of the file list. 
#' 
#' @author Stuart K. Grange
#' 
#' @param path Path/directory where exported Decentlab API files are located. 
#' 
#' @param select Should only a selected set of variables be returned? 
#' 
#' @return Tibble. 
#' 
#' @export
list_decentlab_api_files <- function(path, select = TRUE) {
  
  # Get file list
  file_list <- list.files(path, "decentlab_api_data.csv", full.names = TRUE)
  
  # Get file information for all files and do some string processing
  df <- file_list %>% 
    fs::file_info() %>% 
    mutate(file = fs::path_file(path),
           date = stringr::str_split_fixed(file, "_device", n = 2)[, 1],
           date = lubridate::ymd(date, tz = "UTC"),
           device = stringr::str_split_fixed(file, "device_|_decent", n = 3)[, 2],
           device = as.integer(device)) %>% 
    arrange(device,
            date)
  
  # Only keep a few variables
  if (select) {
    df <- df %>% 
      select(path,
             file,
             size,
             date,
             device)
  }
  
  return(df)
  
}
