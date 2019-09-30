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
#' 
#' The function \code{percentage} is a wrapper around \code{format(as.percentage(...))} with automatic determination of the number of digits, varying between 0 and 1. It also rounds according to basic math rules: \code{percentage(0.4455)} returns \code{"44.6\%"} and not \code{"44.5\%"}. This function always returns a character, and can also be used in plotting, see Examples.
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
#' percentage(0.4455) # rounds to 44.6%
#' 
#' \dontrun{
#' 
#' library(ggplot2)
#' ggplot(data.frame(a = LETTERS[1:6],
#'                   b = runif(6)),
#'        aes(a, b)) +
#'   geom_col() + 
#'   geom_label(aes(label = percentage(b))) +
#'   scale_y_continuous(labels = percentage) 
#' }
as.percentage <- function(x, ...) {
  if (is.percentage(x)) {
    return(x)
  }
  if (!is.numeric(x) & any(grepl("%", x), na.rm = TRUE)) {
    clean_percentage(x, ...)
  } else {
    structure(.Data = as.double(x, ...),
              class = c("percentage", "numeric"))
  }
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


#' @rdname percentage
#' @export
percentage <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- getdecimalplaces(x, minimum = 0, maximum = 1)
  }
  # round right: percentage(0.4455) should return "44.6%", not "44.5%"
  x <- as.numeric(round2(x * 100, digits = digits)) / 100
  format(as.percentage(x), digits = digits, ...)
}
