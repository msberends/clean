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

#' Transform to currency
#' 
#' Transform input to a currency. The actual values are numeric, but will be printed as formatted currency values.
#' @param x input
#' @param currency_symbol the currency symbol to use, which defaults to the current system locale setting
#' @param decimal.mark symbol to use as a decimal separator
#' @param big.mark symbol to use as a thousands separator
#' @param ... other parameters passed on to methods
#' @details Printing currency will always have a currency sign followed by a space, 2 decimal places and is never written in scientific format (like 2.5e+04).
#' @rdname currency
#' @name currency
#' @export
#' @examples 
#' money <- as.currency(c(0.25, 2.5, 25, 25000))
#' money
#' sum(money)
#' max(money)
#' 
#' format(money, currency_symbol = "$")
#' format(money, currency_symbol = "€", decimal.mark = ",")
#' 
#' as.currency(2.5e+04)
as.currency <- function(x, currency_symbol = Sys.localeconv()["currency_symbol"], ...) {
  structure(.Data = as.double(x),
            class = c("currency", "numeric"),
            currency_symbol = unname(currency_symbol))
}

#' @rdname currency
#' @export
is.currency <- function(x) {
  identical(class(x), c("currency", "numeric"))
}

#' @rdname currency
#' @exportMethod print.currency
#' @export
print.currency <- function(x, 
                           decimal.mark = getOption("OutDec"),
                           big.mark = ifelse(decimal.mark == ",", ".", ","),
                           ...) {
  print(trimws(paste0(attributes(x)$currency_symbol, " ",
                      trimws(format(as.numeric(x), 
                                    decimal.mark = decimal.mark, 
                                    big.mark = big.mark,
                                    big.interval = 3L,
                                    nsmall = 2L,
                                    scientific = FALSE)))))
}

#' @rdname currency
#' @exportMethod format.currency
#' @export
format.currency <- function(x, 
                            currency_symbol = Sys.localeconv()["currency_symbol"],
                            decimal.mark = getOption("OutDec"),
                            big.mark = ifelse(decimal.mark == ",", ".", ","),
                            ...) {
  trimws(paste0(currency_symbol, " ",
                trimws(format(as.numeric(x), 
                              decimal.mark = decimal.mark, 
                              big.mark = big.mark,
                              big.interval = 3L,
                              nsmall = 2L,
                              scientific = FALSE))))
}

#' @noRd
#' @exportMethod sum.currency
#' @export
sum.currency <- function(x, ...) {
  as.currency(sum(as.numeric(x), ...))
}

#' @noRd
#' @exportMethod min.currency
#' @export
min.currency <- function(x, ...) {
  as.currency(min(as.numeric(x), ...))
}

#' @noRd
#' @exportMethod max.currency
#' @export
max.currency <- function(x, ...) {
  as.currency(max(as.numeric(x), ...))
}
