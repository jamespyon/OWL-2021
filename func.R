#' # Code for functions
#' Use source() to pull

#' # Import/Update Custom OWL 2021 Data
owl_2021_custom_data <- function(vods) {
  source("import_custom_OWL_2021_data.R")
  if(vods == TRUE) {
    clean_data
  } else {
    clean_data %>% filter(Week != 0)
  }
}

#' Import/Update OWL Player History Data
owl_player_history_data <- function(update, year) {
  #update = TRUE
  #year = c("2018", "2019", "2020", "2021")
  
  # comments
  if(is.na(update)) {"Choose logical TRUE or FALSE"}
  if(is.na(year)) {"Choose from range 2018-2021"}
  
  # create folder if new
  if(!dir.exists("OWL resource")) {dir.create("OWL resource")}
  
  # extract html path
  statslab_site <- "https://overwatchleague.com/en-us/statslab"
  library(rvest)
  html_body_source <- read_html(statslab_site) %>% html_nodes(xpath = "body") %>% as.character() 
  delimited <- html_body_source %>% strsplit(split = "\\", fixed = TRUE) %>% unlist()
  delimited <- delimited[grepl("https://assets.+?zip", delimited)] 
  html_paths <- gsub('\\"', "", delimited)
  
  if(update == TRUE) {
    
    #remove old files
    lapply(year, function(x) {
      all_files <- list.files("OWL resource")
      selected_files <- all_files[grepl(x, all_files)]
      lapply(selected_files, function(x) {
        unlink(paste("OWL resource", x, sep = "/"), recursive = TRUE)
      })
    })
    
    # add new files
    lapply(year, function(x) {
      html <- html_paths[grepl(x, html_paths)]
      download.file(html, destfile = paste("OWL resource/ph_", x, ".zip", sep = ""))
      unzip(paste("OWL resource/ph_", x, ".zip", sep = ""), exdir = paste("OWL resource/ph_", x, sep = ""))
    })
    
  } else {
    
    # open folder
    lapply(year, function(x) {
      selected_path <- paste("OWL resource/ph_", x, sep = "")
      files_list <- list.files(selected_path)
      return(files_list)
    })
    
    
  }

}




