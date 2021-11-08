#' [fn] can only be executed once. Subsequent calls will be noop.
#' @noRd
once <- function(fn) {
  has_run <- FALSE
  function() {
    if (has_run) return(invisible())
    has_run <<- TRUE
    fn()
  }
}

add_class <- \(obj, new_class) `class<-`(obj, c(new_class, class(obj)))
set_attr <- `attr<-`

`%||%` <- \(x, y) if (is.null(x)) y else x
