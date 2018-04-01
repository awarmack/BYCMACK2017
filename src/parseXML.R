##### Parse Yellowbrick XML data ######


library(xml2)
#library(XML)
library(tidyverse)

yb_file<- "./dat/bayviewmack2017.xml"

yb <- read_xml(yb_file)



boats <- xml_find_all(yb, "//Device") %>% xml_attr("name")
 

### For Each Boat ###

get_values <- function(boat_path){
  datues <- xml_children(boat_path) %>% xml_text()
  return(datues)
}

get_boat_data <- function(yb, target_boat){
  # extracts data for a single boat from yellowbrick data
  # Arguements
  #   yb = Yellowbrick data
  #   target_boat = Character name of boat
  
  
  boat_xpath <- paste("//Device[@name=\"", target_boat, "\"]", sep="")
  
  # Get the node with the boat name
  boat_path <- xml_find_all(yb, boat_xpath) 
  
  # Get the column names for each position
  col_names <- xml_child(boat_path) %>% xml_children() %>% xml_name()
  
  boat_positions <- boat_path %>% xml_children()
  
  dat <- lapply(boat_positions, get_values)
  
  dat <- do.call(rbind, dat)
  
  dat <- as.data.frame(dat)
  names(dat) <- col_names
  
  dat$boat <- target_boat
  

  return(dat)
}


clean_boat_data <- function(dat){
  
  numeric_cols <- c("Latitude", "Longitude", "SOG", "COG")
  date_cols <- c("GpsAt")
  
  dat <- dat %>% mutate_all(funs(as.character))
  
  dat <- dat %>% select("boat", date_cols, numeric_cols)
  
  dat <- dat %>% mutate_at(vars(numeric_cols), funs(as.numeric))
  dat$GpsAt <- strptime(test_date, "%FT%T", tz="UTC")
  
  return(dat)
}



zub <- get_boat_data(yb, "Zubenelgenubi") %>% clean_boat_data()

alb <- get_boat_data(yb, "Albacore") %>% clean_boat_data()





