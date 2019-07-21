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

unclean <- data.frame(date = sample(x = c(35000:43000), size = 500, replace = TRUE),
                      gender = sample(x = c("m", "man", "male", "female", "F"), size = 500, 
                                      replace = TRUE, prob = c(0.03, 0.05, 0.51, 0.40, 0.01)),
                      stringsAsFactors = FALSE)

usethis::use_data(unclean, internal = FALSE, overwrite = TRUE, version = 2)
