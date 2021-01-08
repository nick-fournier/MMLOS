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
#' Leave blank as `loadMMLOS()` to use GUI. For demonstration purposes, template data can be loaded by entering dir ="template".
#' The input data format and description are as follows:
#' `r tabular(fread("./data/input_descriptions.csv"))`
#' @return Returns a list containing two data tables for intersections and links.
#' @examples
#' loadMMLOS(dirs = c(intersections = "./data/input_intersection_template.csv", 
#'                  links = "./data/input_link_template.csv"))
#' @export
MMLOSload <- function(dirs) {
  if(missing(dirs)){

    print("Select data file for intersections")
    int.dir = file.choose()
    
    print("Select data file for links (midsegments)")
    link.dir = file.choose()
    
    dirs = c(intersections = int.dir, links = link.dir)
    
  } else if(dirs[1]=="berkeley") {
    
    dirs = c(intersections = "./data/input_intersection_hearstave_template.csv", 
             links = "./data/input_link_hearstave_template.csv")
    
  } else if(dirs[1]=="pasadena") {
    
    dirs = c(intersections = "./data/input_intersection_coloradoblvd_template.csv", 
             links = "./data/input_link_coloradoblvd_template.csv")
  }
  
  #Read data
  dat = lapply(dirs, fread)
  
  #Check order and names
  if(colnames(dat[[2]])[1] == "int_id") dat = rev(dat)
    
  names(dat) = c("INT","LINK")
  
  #Check headers  
  desc <- fread("./data/input_descriptions.csv")
  
  #Check if same amount is there
  chk.len = sapply(names(dat), function(x) length(dat[[x]]) == desc[TYPE == x, .N])
  
  
  msg.len = paste(sapply(names(chk.len[!chk.len]), function(i) {
    paste0("Data does not have the expected number of columns in the '", i, "' data. There are ",
           length(dat[[names(chk.len[!chk.len])]]), " when there should be ", desc[TYPE == i, .N],
           ". Recheck the input data.")
  }), collapse = "\n  ")
  

  try(if(!all(chk.len))
    stop(msg.len)
    )
  
  #check names
  chk.nam = sapply(names(dat), function(x) all(colnames(dat[[x]]) == desc[TYPE == x, VAR]))

  #Formal names
  names(dat) = c("intersections","links")
  
  #Cleanup empty cells
  dat = lapply(dat, function(x) x[ , lapply(.SD, function(y) ifelse(y == "", NA, y))])
  
  #Check for NA
  NAs = sapply(dat, function(x) any(is.na(x)))
  
  #Warning if NAs
  warning(paste0("WARNING: Data contains NAs in ", names(NAs[NAs]), ", make sure this is intentional.\n  "))
  
  #Error with names
  try(if(!all(chk.nam))
    stop(paste0("Data headers do not match expected input format for '", names(chk.nam[!chk.nam]),
                "'! Check file selection or fix file formatting."))
    )
  
  
  class(dat) <- "MMLOS.INPUT"
  
  return(dat)
  
}


#' Save the output somewhere to CSV
#' 
#' @param dat The data you are saving
#' @param output_folder String containing the file output location.
#' @examples
#' saveMMLOS(dat = los_norcal, output_folder = "./outputdata")
#' @export
MMLOSsave <- function(dat, output_folder) {
  if(missing(output_folder)){
    print("Select output location")
    output_folder = choose.dir()
  }
  
  #Read data
  for(n in names(dat)) fwrite(dat[[n]], paste0(output_folder,"/MMLOS_", n,".csv"))


  return(print(paste0("Saved to ", output_folder)))

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


#' Convert LOS score to letter grade
#' 
#' @param score Numeric LOS score 
#' @param mode String for mode being converted. Bike or Pedestrian (future version to include Automobile and Transit)
#' @return A character string for LOS grade.
#' @examples
#' score2LOS(score, mode)
#' @export
score2LOS <- function(score, mode) {
  
  score2LOS <- data.table( LOS = c("A","B","C","D","E","F"),
                           lo_score = c(-Inf,2.00,2.75,3.50,4.25,5.00),
                           hi_score = c(2.00,2.75,3.50,4.25,5.00,Inf))
  # 
  # score2LOS <- data.table( "Inf_60" = c("A","B","C","D","E","F"),
  #                          "40_60"  = c("B","B","C","D","E","F"),
  #                          "24_40"  = c("C","C","C","D","E","F"),
  #                          "15_24"  = c("D","D","D","D","E","F"),
  #                          "8_15"   = c("E","E","E","E","E","F"),
  #                          "-Inf_8" = c("F","F","F","F","F","F"),
  #                          lo_score = c(-Inf,2.00,2.75,3.50,4.25,5.00),
  #                          hi_score = c(2.00,2.75,3.50,4.25,5.00,Inf))
  # 
  # score2LOS <- melt(score2LOS, id.vars = c("lo_score","hi_score"), variable.factor = F)
  # score2LOS[ , c("lo_space", "hi_space") := tstrsplit(variable, "_", fixed = T)]
  # score2LOS[ , (c("lo_space", "hi_space")) := lapply(.SD, as.numeric), .SDcols = c("lo_space", "hi_space")]
  # 
  # if(mode == "Pedestrian")
  #   score2LOS
  # 
  
  return(score2LOS[score > lo_score & score <= hi_score, LOS])
}


#' Calculate the Multi-modal Level of Service
#' 
#' @param input_data List containing data table for intersections and links. See \code{\link{MMLOSload}} for formatting.
#' @param revs Boolean where TRUE calculates LOS with proposed revisions, or FALSE using existing HCM 6th edition.
#' @return A list containing a data tables for bicycle LOS and pedestrian LOS
#' @examples
#' calcMMLOS(input_data, T)
#' @export
MMLOScalc <- function(input_data, revs = T) {
  
  if(class(input_data) != 'MMLOS.INPUT') {
    input_data = MMLOSload(input_data)
  }
  
  #Split by intersection
  int.split <- split(input_data$intersections, by = "int_id")
  link.split <- split(input_data$links, by = c("link_id","link_dir")) 
  
  #Getting LOS score for each link, intersection, and segment.
  if(revs) {
    #Proposed revisions
    LOS = list(
      bike = rbindlist(lapply(link.split, function(link) bike.I_seg(link, int = int.split[[link$boundary_id]], input_data))),
      ped = rbindlist(lapply(link.split, function(link) ped.I_seg(link, int = int.split[[link$boundary_id]], input_data)))
    )
  } else {
    #Existing HCM 
    LOS = list(
      bike = rbindlist(lapply(link.split, function(link) ogbike.I_seg(link, int = int.split[[link$boundary_id]], input_data))),
      ped = rbindlist(lapply(link.split, function(link) ogped.I_seg(link, int = int.split[[link$boundary_id]], input_data)))
    )
  }
  
  class(LOS) <- "MMLOS.OUTPUT"
  
  return(LOS)
}
