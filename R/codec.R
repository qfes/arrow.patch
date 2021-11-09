#' Encode WKB
#'
#' Encodes simple features columns (`sfc`) as well-known binary (WKB) blob
#' columns and stores crs as attributes on these vectors. This allows for
#' serialisation to parquet, ipc_stream and feather.
#' @name encode_wkb
#' @param data <`any`> The input object
#'
#' @author Anthony North
#' @export
encode_wkb <- function(data) {
  UseMethod("encode_wkb")
}

#' @export
encode_wkb.default <- function(data) data

#' @global where
#' @export
encode_wkb.data.frame <- function(data) {
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

#' Decode WKB
#'
#' Decodes well-known binary (WKB) blob columns as simple features columns
#' (`sfc`). This function is useful for decoding wkb columns that were
#' previously encoded with [encode_wkb()].
#'
#' Well-known binary columns are identified by their `wkb` attribute.
#' @name decode_wkb
#' @param data <`any`> The input object
#'
#' @author Anthony North
#' @export
decode_wkb <- function(data) {
  UseMethod("decode_wkb")
}

#' @export
decode_wkb.default <- function(data) data

#' @global where
#' @export
decode_wkb.data.frame <- function(data) {
  is_sfc <- \(col) attr(col, "wkb") %||% FALSE
  from_wkb <- \(col) sf::st_as_sfc(col, crs = attr(col, "crs"))

  data |>
    dplyr::mutate(dplyr::across(where(is_sfc), from_wkb))
}
