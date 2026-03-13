#' Pulls out the day as a number
#'
#' This extracts a day from a date
#' @param x the date
#' @param fmat is the date format
#' @return Returns the day
#' @examples
#' x <- "2014-02-14"
#' as.day(x)
#' @export
as.day <-   function(x,fmat='%Y-%m-%d')    as.numeric(format(as.Date(x, format=fmat),'%d'))


#' Pulls out the month as a number
#'
#' This extracts a month from a date
#' @param x the date
#' @param fmat is the date format
#' @return Returns the month
#' @examples
#' x <- "2014-02-14"
#' as.month(x)
#' @export
as.month <- function(x,fmat='%Y-%m-%d')    as.numeric(format(as.Date(x, format=fmat),'%m'))

#' Pulls out the year as a number
#'
#' This extracts a year from a date
#' @param x the date
#' @param fmat is the date format
#' @return Returns the year
#' @examples
#' x <- "2014-02-14"
#' as.year(x)
#' @export
as.year <-  function(x,fmat='%Y-%m-%d')    as.numeric(format(as.Date(x, format=fmat),'%Y'))
