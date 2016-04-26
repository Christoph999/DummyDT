generate_routes_dt <- function(nmax) {
  library(data.table)
  message("generate_routes_dt...")
  routes <- data.table::data.table(lat1 = numeric(nmax),
    lng1 = numeric(nmax),
    lat2 = numeric(nmax),
    lng2 = numeric(nmax),
    time = numeric(nmax))#, stringsAsFactors = F) not needed for data.table
  tmp <- sample(seq(46, 49, length.out = 2*nmax), nmax)
  routes$lat1 <- tmp
  tmp <- sample(seq(8, 10, length.out = 2*nmax), nmax)
  routes$lng1 <- tmp
  tmp <- sample(seq(46, 49, length.out = 2*nmax), nmax)
  routes$lat2 <- tmp
  tmp <- sample(seq(8, 10, length.out = 2*nmax), nmax)
  routes$lng2 <- tmp
  tmp <- sample(seq(0, 1e7, length.out = 2*nmax), nmax)
  routes$time <- as.integer(tmp)
  routes <- unique(routes)
  routes <- routes[order(lat1, lng1, lat2, lng2, time)]
  data.table::setattr(routes, "sorted", c("lat1", "lng1", "lat2", "lng2"))
  data.table::setkey(routes, lat1, lng1, lat2, lng2)
  search_route <- routes[sample(seq_len(nrow(routes)))[1], ]
  return(list(routes = routes, search_route = search_route))
}
