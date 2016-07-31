#' Makes a timestamp in the format Sean likes
#'
#' In all the figures and files I make during analysis, I add a timestamp prefix of the 
#' format "YYMMDD". That way, I can sort by filename and have them in chronolgical order,
#' and not have to worry about the timestamps created by the filesystem.
#' 
#' @keywords date time
#' @export
notebookTimestamp <- function() {
    today <- as.character(Sys.Date())
    
    #Snip out the '20' part of the year.
    #I'm committed to the 21st century.
    today <- substr(today, 3, nchar(today))
    today <- unlist(strsplit(today, "-"))
    return(paste(today, collapse=""))
}


#' Adds a timestamp to a filename
#'
#' Quick way to add one of Sean's Preferred Timestamps (TM) to a figure filename.
#' 
#' @keywords date time
#' @param title The filename to output
#' @export
figureFilename <- function(title) {
    return(paste(notebookTimestamp(), title, sep="_"))
}
