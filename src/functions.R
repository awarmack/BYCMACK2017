#Functions


convCoord <- function(coord){
  #takes a coordinate in the system xx xx.xxxx and converts it to xx.xxxxxx
  
  if(substr(coord, 1, 1)=="0"){
    #it's a longitude
    deg <- as.numeric(substr(coord, 1, 3))
    min <- substr(coord, 4, nchar(coord))
    min <- as.numeric(min)
    min <- min/60
    out <- deg + min
    
    
  } else {
    #it's a latitude
    deg <- as.numeric(substr(coord, 1, 2))
    min <- substr(coord, 3, nchar(coord))
    min <- as.numeric(min)
    min <- min/60
    out <- deg + min
  }
  
  return(out)
  
}