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
#' @param x Data frame or matrix to copy.
#' @param rnames Logical. Include row names? Default \code{FALSE}.
#' @param big Logical. Use a larger clipboard buffer (Windows only,
#'   increases from default ~32KB to ~4MB)? Default \code{FALSE}.
#' @return Clipboard gets loaded up
#' @examples
#' dat <- data.frame(year=2000:2021, temperature=23)
#' toXL(dat)
#' @export
toXL <- function(x, rnames = FALSE, big = FALSE) {
  if (.Platform$OS.type == "windows") {
    cb <- if (big) "clipboard-4194304" else "clipboard"
    utils::write.table(x, cb, sep = "\t", row.names = rnames)
  } else if (Sys.info()["sysname"] == "Darwin") {
    con <- pipe("pbcopy")
    utils::write.table(x, con, sep = "\t", row.names = rnames)
    close(con)
  } else {
    con <- pipe("xclip -selection clipboard")
    utils::write.table(x, con, sep = "\t", row.names = rnames)
    close(con)
  }
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
