test_that("test function with DT", {

  # Create search task with generated data.table
  nmax <- 10
  DT <- generate_routes_dt(nmax)
  # browser()
  unknown <- DT$search_route
  routes <- DT$routes
  res <- find_routes(routes, unknown$lat1, unknown$lng1, unknown$lat2, unknown$lng2)
  print("Done")
})
