test_that("read & write works", {
  arrow.patch::patch_parquet()

  df <- dplyr::as_tibble(mtcars) |>
    dplyr::mutate(
      sf_col = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
    )

  parquet_file <- tempfile()
  arrow::write_parquet(df, parquet_file)
  df2 <- arrow::read_parquet(parquet_file)

  # round trip worked
  expect_equal(df, df2)

  # this is redundant
  sf <- sf::st_as_sf(df)
  arrow::write_parquet(sf, parquet_file)

  sf2 <- arrow::read_parquet(parquet_file)

  # round trip worked
  expect_equal(sf, sf2)
})
