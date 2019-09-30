# ==================================================================== #
# TITLE                                                                #
# Fast and Easy Data Cleaning                                          #
#                                                                      #
# SOURCE                                                               #
# https://github.com/msberends/clean                                   #
#                                                                      #
# LICENCE                                                              #
# (c) 2019 Berends MS (m.s.berends@umcg.nl)                            #
#                                                                      #
# This R package is free software; you can freely use and distribute   #
# it for both personal and commercial purposes under the terms of the  #
# GNU General Public License version 2.0 (GNU GPL-2), as published by  #
# the Free Software Foundation.                                        #
#                                                                      #
# This R package was publicly released in the hope that it will be     #
# useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ==================================================================== #

#' Transform to percentage
#' 
#' Transform input to a percentage. The actual values are numeric, but will be printed as formatted percentages.
#' @param x input
#' @param ... other parameters passed on to methods
#' @param digits how many digits should be printed. It defaults to printing all decimals available in the data after transforming to a percentage, with a minimum of 0 and a maximum of 3.
#' @details Printing percentages will always have a percentage symbol and is never written in scientific format (like 2.5e+04\%).
#' @rdname percentage
#' @name percentage
#' @export
#' @examples 
#' proportion <- as.percentage(c(0.25, 2.5, 0.0025))
#' proportion
#' sum(proportion)
#' max(proportion)
#' mean(proportion)
#' 
#' as.percentage(2.5e-14)
#' 
#' as.percentage(pi)
#' format(as.percentage(pi))
#' format(as.percentage(pi), digits = 6)
as.percentage <- function(x, ...) {
  if (is.percentage(x)) {
    return(x)
  }
  structure(.Data = as.double(x, ...),
            class = c("percentage", "numeric"))
}

#' @noRd
#' @exportMethod as.double.percentage
#' @export
as.double.percentage <- function(x, ...) {
  as.double(structure(x, class = "numeric"))
}

#' @rdname percentage
#' @export
is.percentage <- function(x) {
  identical(class(x), c("percentage", "numeric"))
}

#' @exportMethod [.percentage
#' @export
#' @noRd
"[.percentage" <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}
#' @exportMethod [<-.percentage
#' @export
#' @noRd
"[<-.percentage" <- function(value) {
  y <- NextMethod()
  attributes(y) <- attributes(value)
  y
}
#' @exportMethod [[.percentage
#' @export
#' @noRd
"[[.percentage" <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}
#' @exportMethod [[<-.percentage
#' @export
#' @noRd
"[[<-.percentage" <- function(value) {
  y <- NextMethod()
  attributes(y) <- attributes(value)
  y
}
#' @exportMethod c.percentage
#' @export
#' @noRd
c.percentage <- function(x, ...) {
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}

#' @rdname percentage
#' @exportMethod print.percentage
#' @export
print.percentage <- function(x, ...) {
  print(format(x), quote = FALSE)
}

getdecimalplaces <- function(x, minimum = 0, maximum = 3) {
  max_places <- max(unlist(lapply(strsplit(sub('0+$', '', 
                                               as.character(x * 100)), ".", fixed = TRUE),
                                  function(y) ifelse(length(y) == 2, nchar(y[2]), 0))), na.rm = TRUE)
  max(min(max_places,
          maximum, na.rm = TRUE),
      minimum, na.rm = TRUE)
}

#' @rdname percentage
#' @exportMethod format.percentage
#' @export
format.percentage <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- getdecimalplaces(x)
  }
  x_formatted <- format(as.double(x) * 100, scientific = FALSE, digits = digits, nsmall = digits, ...)
  x_formatted[!is.na(x)] <- paste0(x_formatted[!is.na(x)], "%")
  x_formatted
}

#' @noRd
#' @exportMethod sum.percentage
#' @export
sum.percentage <- function(x, ...) {
  as.percentage(sum(as.double(x), ...))
}

#' @noRd
#' @exportMethod min.percentage
#' @export
min.percentage <- function(x, ...) {
  as.percentage(min(as.double(x), ...))
}

#' @noRd
#' @exportMethod max.percentage
#' @export
max.percentage <- function(x, ...) {
  as.percentage(max(as.double(x), ...))
}

#' @noRd
#' @exportMethod mean.percentage
#' @export
mean.percentage <- function(x, ...) {
  as.percentage(mean(as.double(x), ...))
}

#' @noRd
#' @exportMethod summary.percentage
#' @export
summary.percentage <- function(object, ...) {
  c("Class" = 'percentage',
    "<NA>" = length(object[is.na(object)]),
    "Min." = format(min(object)),
    "Mean" = format(mean(object)),
    "Max." = format(max(object)))
}

#' @importFrom pillar type_sum
#' @export
type_sum.percentage <- function(x) {
  "pct"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.percentage <- function (x, ...) {
  pillar_shaft(as.numeric(x) * 100, ...)
}
