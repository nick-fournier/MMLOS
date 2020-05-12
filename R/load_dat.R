#These functions load the necessary input data


#Loads in the data
func.loaddat <- function(dirs) {
  if(missing(dirs)){
    
    print("Select data file for intersections")
    int.dir = file.choose()
    
    print("Select data file for links (midsegments)")
    link.dir = file.choose()
  } else if(dirs=="template") {
    
    int.dir = "./data/input_intersection_template.csv"
    link.dir = "./data/input_link_template.csv"
    
  }
  
  dat = lapply(list(int.dir,link.dir), fread)
  names(dat) = c("INT","LINK")
  
  #Check headers  
  desc <- fread("./data/input_descriptions.csv")
  chk = sapply(c("INT","LINK"), function(x) all(colnames(dat[[x]]) == desc[TYPE == x, VAR]))
  
  try(if(!all(chk))
    stop(paste0("Data does not match expected input format for '", names(chk[!chk]),"'! Check file selection or fix file formatting."))
    )
  
  return(dat)
  
}

