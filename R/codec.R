encode <- function(data) {
  UseMethod("encode")
}

#' default -> identity
#' @noRd
encode.default <- function(data) data

#' convert all sfc columns to wkb
#' @global where
#' @noRd
encode.data.frame <- function(data) {
  is_sfc <- \(col) inherits(col, "sfc")

  # convert to wkb, patch class for arrow, add crs
  as_wkb <- function(col) {
    sf::st_as_binary(col) |>
      unclass() |>
      blob::new_blob() |>
      add_class("arrow_binary") |>
      set_attr("crs", sf::st_crs(col)) |>
      set_attr("wkb", TRUE)
  }

  # convert all sfc columns (if there are any) to wkb
  data |>
    dplyr::mutate(dplyr::across(where(is_sfc), as_wkb))
}

decode <- function(data) {
  UseMethod("decode")
}

#' default -> identity
#' @noRd
decode.default <- function(data) data

#' convert all wkb columns to sfc
#' @global where
#' @noRd
decode.data.frame <- function(data) {
  is_sfc <- function(col) attr(col, "wkb") %||% FALSE
  from_wkb <- \(col) sf::st_as_sfc(col, crs = attr(col, "crs"))

  data |>
    dplyr::mutate(dplyr::across(where(is_sfc), from_wkb))
}
