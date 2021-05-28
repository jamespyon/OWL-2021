#' # Library
library(tidyverse)

#' # Import Custom Overwatch League 2021 Datasheet
initial_import <- readxl::read_xlsx("D:\\Documents\\Overwatch League 2021.xlsx")

#' ## Filter for usable data
clean_data <- initial_import %>% filter(!is.na(WL)) %>% arrange(Frame, Week, Vs, Date)
