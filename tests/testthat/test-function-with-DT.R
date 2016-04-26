test_that("test function with DT", {
  library(data.table)

  # Create search task with generated data.table
  nmax <- 10
  DT <- generate_routes_dt(nmax)
  print("Done")
})
