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

context("percentage.R")

test_that("percentage works", {
  expect_identical(as.percentage(as.numeric(as.percentage(c(0.25, 25, 2.5)))),
                   as.percentage(c(0.25, 25, 2.5)))
  expect_true(is.percentage(clean_percentage(c("no5.538", "no.929", "yes23.90", "no.841", "no2.610"))))
})

