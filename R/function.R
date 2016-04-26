

#' Title
#'
#' @param nmax
#'
#' @return nmax
#' @export generate_routes_dt
#' @import data.table
#'
generate_routes_dt <- function(nmax) {
  library(data.table)
  message("generate_routes_dt...")
  routes <- data.table::data.table(lat1 = numeric(nmax),
    lng1 = numeric(nmax),
    lat2 = numeric(nmax),
    lng2 = numeric(nmax),
    time = numeric(nmax))#, stringsAsFactors = F) not needed for data.table
  set.seed(1)
  tmp <- sample(seq(46, 49, length.out = 2*nmax), nmax)
  routes$lat1 <- tmp
  set.seed(2)
  tmp <- sample(seq(8, 10, length.out = 2*nmax), nmax)
  routes$lng1 <- tmp
  # set.seed(3)
  tmp <- sample(seq(46, 49, length.out = 2*nmax), nmax)
  routes$lat2 <- tmp
  set.seed(4)
  tmp <- sample(seq(8, 10, length.out = 2*nmax), nmax)
  routes$lng2 <- tmp
  set.seed(5)
  tmp <- sample(seq(0, 1e7, length.out = 2*nmax), nmax)
  routes$time <- as.integer(tmp)
  routes <- unique(routes)
  routes <- routes[order(lat1, lng1, lat2, lng2, time)]
  data.table::setattr(routes, "sorted", c("lat1", "lng1", "lat2", "lng2"))
  data.table::setkey(routes, lat1, lng1, lat2, lng2)
  set.seed(6)
  search_route <- routes[sample(seq_len(nrow(routes)))[1], ]
  return(list(routes = routes, search_route = search_route))
}

#' find_routes
#'
#' @param routes A data.frame with columns lat1, lng1, lat2, lng2, time
#' @param lat1 A numeric, latitude of position 1
#' @param lng1 A numeric, longitude of position 1
#' @param lat2 A numeric, latitude of position 2
#' @param lng2 A numeric, longitude of position 2
#' @param prec An integer, precision to decide whether two routes coincide or not.
#' @import data.table
#'
#' @return routes list
find_routes <- function(routes, lat1, lng1, lat2, lng2){
  args <- c(lat1,lng1,lat2,lng2)
  assertthat::assert_that(is.numeric(args))
  found_route <- routes[J(args), nomatch = 0L]
#   assertthat::assert_that(is.numeric(c(lat1, lng1, lat2, lng2)))
#   found_route <- routes[.(lat1, lng1, lat2, lng2), nomatch = 0L] # ............. Does not work: lat1 = routes$lat1!?!?!?
  return(found_route)
}

