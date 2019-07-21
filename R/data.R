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

#' Example data that is not clean
#' 
#' This typical data example can be used for checking and cleaning.
#' @format A \code{\link{data.frame}} with 500 observations and the following variables:
#' \describe{
#'   \item{\code{date}}{Dates imported from Excel, they are integers ranging from ~30,000 to ~43,000.}
#'   \item{\code{gender}}{Characters with mixed values observed in original data about patients gender.}
#' }
#' @seealso \code{\link{freq}} to check values and \code{\link{clean}} to clean them.
"unclean"
