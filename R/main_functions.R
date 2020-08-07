# These are the main scripts, which call the subscripts.



#Turns data frame into tabular
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  
  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))
  
  header = paste(paste(colnames(df), collapse = " \\tab "), "\\cr\n ")
  
  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
        header,
        contents, "\n}\n", sep = "")
}

#' Load the link and intersection data into R
#' 
#' @param dirs String vector containing the file locations of two CSV files for intersection and link data.
#' For demonstration purposes, template data can be loaded by entering dir ="template".
#' The input data format and description are as follows:
#' `r tabular(fread("./data/input_descriptions.csv"))`
#' @return Returns a list containing two data tables for intersections and links.
#' @examples
#' loaddat(dirs = c(intersections = "./data/input_intersection_template.csv", 
#'                  links = "./data/input_link_template.csv"))
#' 
loaddat <- function(dirs) {
  if(missing(dirs)){

    print("Select data file for intersections")
    int.dir = file.choose()
    
    print("Select data file for links (midsegments)")
    link.dir = file.choose()
    
    dirs = c(intersections = int.dir, links = link.dir)
    
  } else if(dirs[1]=="template") {
    
    dirs = c(intersections = "./data/input_intersection_template.csv", 
                links = "./data/input_link_template.csv")
  }
  
  #Read data
  dat = lapply(dirs, fread)
  
  #Check order and names
  if(colnames(dat[[2]])[1] == "int_id")
    dat = rev(dat)
    
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

#Leftover load function... might repurpose it.
loadtab <- function() {
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
score2LOS <- function(score) {
  
  score2LOS <- data.table( LOS = c("A","B","C","D","E","F"), 
                           lo_score = c(-Inf,2.00,2.75,3.50,4.25,5.00),
                           hi_score = c(2.00,2.75,3.50,4.25,5.00,Inf))
  
  return(score2LOS[score > lo_score & score <= hi_score, LOS])
}


#' Calculate the Multimodal Level of Service
#' 
#' @param dat List containing data table for intersections and links. See \code{\link{loaddat}} for formatting.
#' @param revs Boolean where TRUE calculates LOS with proposed revisions, or FALSE using existing HCM 6th edition.
#' @return A list containing a data tables for bicycle LOS and pedestrian LOS
#' @examples
#' MMLOS(dat, T)

MMLOS <- function(dat, revs = T) {
  #Split by intersection
  int.split <- split(dat$intersections, by = "int_id")  
  link.split <- split(dat$links, by = c("link_id","link_dir")) 
  
  #Getting LOS score for each link, intersection, and segment.
  if(revs) {
    #Proposed revisions
    LOS = list(
      bike = rbindlist(lapply(link.split, function(link) bike.I_seg(link, int = int.split[[link$boundary_id]]))),
      ped = rbindlist(lapply(link.split, function(link) ped.I_seg(link, int = int.split[[link$boundary_id]])))
    )
  } else {
    #Existing HCM 
    LOS = list(
      bike = rbindlist(lapply(link.split, function(link) ogbike.I_seg(link, int = int.split[[link$boundary_id]]))),
      ped = rbindlist(lapply(link.split, function(link) ogped.I_seg(link, int = int.split[[link$boundary_id]])))
    )
  }

  return(LOS)
}
