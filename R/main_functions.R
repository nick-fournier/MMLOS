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
  names(dat) = c("intersections","links")
  
  #Check headers  
  desc <- fread("./data/input_descriptions.csv")
  chk = sapply(c("intersections","links"), function(x) all(colnames(dat[[x]]) == desc[TYPE == x, VAR]))
  
  #Cleanup empty cells
  dat = lapply(dat, function(x) x[ , lapply(.SD, function(y) ifelse(y == "", NA, y))])
  
  #Check for NA
  NAs = sapply(dat, function(x) any(is.na(x)))
  
  print(paste0("WARNING: Data contains NAs in ", names(NAs[NAs]), ", make sure this is intentional"))
  
  
  try(if(!all(chk))
    stop(paste0("Data does not match expected input format for '", names(chk[!chk]),"'! Check file selection or fix file formatting."))
    )
  
  return(dat)
  
}

func.loadtab <- function() {
  list(
    #Load approx turn delay table
    turndelay = melt(fread("./data/turning_vehicle_delay.csv"), id.vars = "v_m"),
  
    #Load approx delay equivalent LOS 
    delaylos = fread("./data/ped_delay_los.csv"),
    
    #Segment LOS grade
    seglos = fread("./data/ped_seg_los.csv"),
    
    #Link LOS grade
    linklos = fread("./data/ped_link_los.csv"),
    
    #Ped LOS grade
    score2los = fread("./data/score_to_los.csv")
  )
}


#LOS Grade from score
func.score2LOS <- function(score) {
  tab$score2los[score > lo_score & score <= hi_score, LOS]
}

#Pedestrian LOS
func.MMLOS <- function(dat) {
  
  #Split by intersection
  int.split <- split(dat$intersections, by = "int_id")  
  link.split <- split(dat$links, by = c("link_id","link_dir")) 
  
  #Getting LOS score for each link, intersection, and segment.
  bike.LOS = rbindlist(lapply(link.split, function(link)
    func.bike.I_seg(link, int = int.split[[link$boundary_id]])
  ))
  
  
  ped.LOS = rbindlist(lapply(link.split, function(link) 
    func.ped.I_seg(link, int = int.split[[link$boundary_id]])
  ))
  
  #Create output list
  LOS = list(bikes = bike.LOS,
             pedestrians = ped.LOS)
  
  return(LOS)
}
