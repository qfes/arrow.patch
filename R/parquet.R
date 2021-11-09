#' Patch parquet
#'
#' Replaces [arrow::read_parquet()] and [arrow::write_parquet()]
#' [arrow.patch::read_parquet()] and [arrow.patch::write_parquet()]
#' @author Anthony North
#' @include utils.R
#' @export
patch_parquet <- once(function() {
  write_parquet <- arrow::write_parquet
  read_parquet <- arrow::read_parquet

  assignInNamespace("write_parquet", ns = "arrow", function(x, ...) {
    write_parquet(encode_wkb(x), ...)
  })

  assignInNamespace("read_parquet", ns = "arrow", function(...) {
    read_parquet(...) |>
      decode_wkb()
  })
})
