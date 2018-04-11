#analyze 
rm(list=ls())

yb_xml_files <- list.files("./dat", full.names = TRUE)
yb_xml_files <- yb_xml_files[grep(".xml", yb_xml_files)]

source("./src/parseXML.R")

dat <- lapply(yb_xml_files[2:4], parse_yb_XML)
