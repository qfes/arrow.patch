test_that("encode works", {
  df <- dplyr::tibble(
    other_col = 1,
    sf_col = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )

  encoded_df <- encode_wkb(df)

  # preserve attributes
  expect_equal(attributes(encoded_df), attributes(df))

  # non-sfc cols unchanged
  expect_equal(encoded_df$other_col, df$other_col)

  # sfc cols are encoded as wkb
  expect_s3_class(encoded_df$sf_col, c("arrow_binary"))
  # crs preserved
  expect_equal(sf::st_crs(df$sf_col), attr(encoded_df$sf_col, "crs"))
  # wkb attribute for decoding
  expect_true(attr(encoded_df$sf_col, "wkb"))
})

test_that("decode works", {
  sf_col <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  # encode as wkb
  wkb_col <- sf::st_as_binary(sf_col)
  wkb_col <- blob::new_blob(unclass(wkb_col))
  class(wkb_col) <- c("arrow_binary", class(wkb_col))
  attr(wkb_col, "crs") <- sf::st_crs(sf_col)
  attr(wkb_col, "wkb") <- TRUE

  encoded_df <- dplyr::tibble(
    other_col = 1,
    sf_col = wkb_col
  )

  decoded_df <- decode_wkb(encoded_df)

  # attributes preserved
  expect_equal(attributes(decoded_df), attributes(encoded_df))

  # non-sfc cols unchanged
  expect_equal(encoded_df$other_col, decoded_df$other_col)

  # sfc col decoded
  expect_s3_class(decoded_df$sf_col, "sfc")
  expect_equal(sf::st_crs(decoded_df$sf_col), sf::st_crs(4326))
})
