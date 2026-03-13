#' Change factor to number
#'
#' This Changes a factor to number
#' @param x the factor
#' @return Returns a number
#' @examples
#' x <- as.factor(88)
#' numchar(x)
#' @export
numchar <-  function(x) as.numeric(as.character(x))

#' Copy to Clipboard
#'
#' Tkes a dataframe and copies to to the clipboard so you can paste it into Excel
#' @param x name of data.frame
#' @param rnames logical - should row names be printed
#' @return Clipboard gets loaded up
#' @examples
#' dat <- data.frame(year=2000:2021, temperature=23)
#' toXL(dat)
#' @export
toXL <- function(x, rnames=FALSE){
  utils::write.table(x, "clipboard", sep="\t", row.names = rnames)
}

#' Opens a text file and converts to data.frames
#' @param x the text file name
#' @return Returns a data.frame
#' @examples
#' OpenText(system.file("extdata", "test.txt", package = "LobFunc"))
#' @export
OpenText <- function(x) {
  lines <- readLines(x)
  split_lines <- strsplit(lines, "\\s+")
  names <- sapply(split_lines, `[`, 1)
  values <- lapply(split_lines, function(x) as.numeric(x[-1]))
  max_len <- max(sapply(values, length))
  padded <- lapply(values, function(x) c(x, rep(NA, max_len - length(x))))
  df <- as.data.frame(padded)
  names(df) <- names
  return(df) }
