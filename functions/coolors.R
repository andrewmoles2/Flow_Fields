# https://coolors.co/
coolors <- function(URL){
  # function takes coolors url, extracts hex codes and adds #
  # detects if from default or explored palettes
  if (grepl("palette", URL) == TRUE) {
    # extract just the hex
    cstr <- gsub("coolors.co", "", URL)
    cstr <- gsub("https:///", "", cstr)
    cstr <- gsub("palette/", "", cstr)
    cstr <- gsub("-", " ", cstr)
    
    # split into individual strings and add #
    cvec <- strsplit(cstr, " ")
    cvec <- paste0("#", cvec[[1]])
    
    return(cvec)
    
  } else {
    # extract just the hex
    cstr <- gsub("coolors.co", "", URL)
    cstr <- gsub("https:///", "", cstr)
    cstr <- gsub("-", " ", cstr)
    
    # split into individual strings and add #
    cvec <- strsplit(cstr, " ")
    cvec <- paste0("#", cvec[[1]])
    
    return(cvec)
  }
}
