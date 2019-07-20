# ==================================================================== #
# TITLE                                                                #
# Functions to Clean Data Sets                                         #
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
# This R package was created for academic research and was publicly    #
# released in the hope that it will be useful, but it comes WITHOUT    #
# ANY WARRANTY OR LIABILITY.                                           #
# ==================================================================== #

#' Regular expressions for \code{TRUE} and \code{FALSE}
#' 
#' These functions just return a regular expression to define values \code{TRUE} and \code{FALSE} in the most spoken languages in the world. They are the default input for the function \code{\link{clean_logical}}.
#' @details 
#' Both functions support values "Yes" and "No" in the following languages: Arabic, Bengali, Chinese (Mandarin), Dutch, English, French, German, Hindi, Indonesian, Japanese, Malay, Portuguese, Russian, Spanish, Telugu, Turkish and Urdu. 
#' 
#' Note: all these translations are in Latin characters only (e.g. "da" for Russian, "haan" for Hindi and "hai" for Japanese).
#' @source Wolfram Alpha, query: \url{https://www.wolframalpha.com/input/?i=20+most+spoken+languages}
#' @name regex_true_false
#' @rdname regex_true_false
#' @export
regex_true <- function() {
  "^(true|t|[1-9]|shi|dui|yes|haan|si|da|sim|ya|ha|naam|iva|hai|oui|ja|han-ji|evet|avunu)([^a-z]+?.*)?$"
}

#' @rdname regex_true_false
#' @export
regex_false <- function() {
  "^(false|f|0|pu shi|no|nahin|niet|nao|tidak|na|laa|iie|non|nein|hayir|kadhu|nee)([^a-z]+?.*)?$"
}
