##' Build a multline string
##'
##' @title Build a multline string
##' @param ... String fragments to build the string from
##' @export
##' @examples
##'
##' mls::mls("a string that is", "composed of bits of strings")
##' mls::mls("a string that contains", "", "line breaks")
mls <- function(...) {
  ## TODO: option to control preseving whitespace
  ## TODO: option to trim whitespace to initial indent (possible?)
  ret <- vapply(list(...), as.character, "")
  class(ret) <- "mls"
  ret
}


##' @export
as.character.mls <- function(x, ...) {
  format(x)
}


##' @export
format.mls <- function(x, width = getOption("width") * 0.9, ...,
                       append_newline = FALSE) {
  x <- paste(x, collapse = "\n")
  join_newline(strwrap(x, width), append_newline)
}


##' @export
print.mls <- function(x, ...) {
  cat(format(x, append_newline = TRUE))
  invisible(x)
}


join_newline <- function(x, append_newline = FALSE) {
  if (append_newline) {
    paste0(x, "\n", collapse = "")
  } else {
    paste0(x, collapse = "\n")
  }
}
