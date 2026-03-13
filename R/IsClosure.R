# R/fishing_closures.R

# ---------------------------------------------------------------------------
# Internal closure polygon definitions
# ---------------------------------------------------------------------------

.closure_polys <- list(
  bigbankall = data.frame(
    lat = c(-27.03476667, -27.03476667, -27.5, -27.5),
    lon = c(113.04306667, 113.23055, 113.3661333, 113.1780167)
  ),
  bigbankcomm = data.frame(
    lat = c(-27.09167, -27.16667, -27.16667, -27.09167),
    lon = c(113.2833, 113.2833, 113.0333, 113.0333)
  ),
  bigbanksouth = data.frame(
    lat = c(-27.233, -27.233, -27.5, -27.5),
    lon = c(113.1043, 113.2868, 113.3661333, 113.1780167)
  ),
  bigbanknorth = data.frame(
    lat = c(-27.233, -27.233, -27.034767, -27.034767),
    lon = c(113.1043, 113.2868, 113.2307, 113.043067)
  ),
  leeman = data.frame(
    lat = c(-29.952666, -29.952667, -30.047667, -30.047667),
    lon = c(114.52933, 114.56767, 114.60467, 114.56633)
  )
)

#' Closure period lookup table
#'
#' A data frame recording the start and end years during which each named
#' polygon was closed to fishing.  \code{bigbanknorth} is intentionally omitted
#' (no commercial closure period defined).
#'
#' @format A data frame with columns \code{startyr}, \code{endyr}, \code{area}.
#' @keywords internal
.closure_times <- data.frame(
  startyr = c(2008, 2020, 2012, 2008),
  endyr   = c(2019, 2024, 2100, 2100),
  area    = c("bigbankall", "bigbanksouth", "bigbankcomm", "leeman"),
  stringsAsFactors = FALSE
)

# Build the combined sf object once at package load — fast lookup at runtime
.build_closures_sf <- function() {
  polys_sf <- do.call(rbind, lapply(names(.closure_polys), function(nm) {
    df     <- .closure_polys[[nm]]
    coords <- rbind(as.matrix(df[, c("lon", "lat")]),
                    as.matrix(df[1,  c("lon", "lat")]))  # close ring
    sf::st_sf(
      area     = nm,
      geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
    )
  }))
  merge(polys_sf, .closure_times, by = "area", all.x = TRUE)
}

# Package-level cache — populated by .onLoad, never exported
.closures_sf <- NULL

.onLoad <- function(libname, pkgname) {
  .closures_sf <<- .build_closures_sf()
}


# ---------------------------------------------------------------------------
# Exported function
# ---------------------------------------------------------------------------

#' Test whether spatial points were closed to fishing
#'
#' Given vectors of longitude, latitude and year, returns a logical vector
#' indicating whether each point falls inside a defined fishing closure polygon
#' that was active during the specified year.  The underlying \code{sf} polygon
#' object is built once at package load, so repeated calls on large vectors are
#' efficient.
#'
#' @param lons Numeric vector of longitudes (decimal degrees, WGS84).
#' @param lats Numeric vector of latitudes (decimal degrees, WGS84).
#'   Must be the same length as \code{lons}.
#' @param year Numeric vector of years to test.  Recycled to the length of
#'   \code{lons} if a scalar is supplied.
#'
#' @return A logical vector of the same length as \code{lons}.  \code{TRUE}
#'   indicates that the point was inside an active closure polygon during
#'   \code{year}; \code{FALSE} indicates it was open (or outside all polygons).
#'
#' @details
#' Defined closure areas and their active periods are:
#' \tabular{lll}{
#'   \strong{Area}    \tab \strong{Start} \tab \strong{End} \cr
#'   bigbankall       \tab 2008           \tab 2019          \cr
#'   bigbanksouth     \tab 2020           \tab 2024          \cr
#'   bigbankcomm      \tab 2012           \tab ongoing       \cr
#'   leeman           \tab 2008           \tab ongoing       \cr
#'   bigbanknorth     \tab —              \tab —             \cr
#' }
#' \code{bigbanknorth} is defined spatially but has no associated closure
#' period and will always return \code{FALSE}.
#'
#' @examples
#' # Point inside Big Bank All closure during its active period
#' is_closed(lons = 113.15, lats = -27.30, year = 2015)  # TRUE
#'
#' # Same point after the closure ended
#' is_closed(lons = 113.15, lats = -27.30, year = 2022)  # FALSE
#'
#' # Vectorised use — typical application to a data frame
#' \dontrun{
#' df$closed <- is_closed(df$lon, df$lat, df$year)
#' }
#'
#' @seealso \code{\link[sf]{st_intersects}}
#'
#' @importFrom sf st_as_sf st_intersects
#' @export
is_closed <- function(lons, lats, year) {
  if (length(lons) != length(lats))
    stop("`lons` and `lats` must be the same length.")

  pts  <- sf::st_as_sf(data.frame(lon = lons, lat = lats),
                       coords = c("lon", "lat"), crs = 4326)
  year <- rep_len(year, nrow(pts))

  intersects <- sf::st_intersects(pts, .closures_sf, sparse = FALSE)

  mapply(function(in_poly, yr) {
    if (!any(in_poly)) return(FALSE)
    matched <- .closures_sf[in_poly, ]
    any(!is.na(matched$startyr) &
          yr >= matched$startyr &
          yr <= matched$endyr)
  }, asplit(intersects, 1), year)
}
