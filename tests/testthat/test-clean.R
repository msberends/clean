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

context("clean.R")

test_that("cleaning works", {
  expect_equal(clean_logical(c("Yes", "No", "Invalid", "Unknown")),
               c(TRUE, FALSE, NA, NA))
  expect_equal(clean_logical(x = c("Positive", "Negative", "Unknown", "Some value"),
                             true = "pos",
                             false = "neg"),
               c(TRUE, FALSE, NA, NA))
  
  gender_age <- c("male 0-50", "male 50+", "female 0-50", "female 50+")
  expect_equal(clean_factor(gender_age, levels = c("M", "F")),
               factor(c("M", "M", "F", "F"), levels = c("M", "F")))
  expect_equal(clean_factor(gender_age, levels = c("Male", "Female")),
               factor(c("Male", "Male", "Female", "Female"), levels = c("Male", "Female")))
  
  expect_equal(clean_factor(gender_age, levels = c("0-50", "50+"), ordered = TRUE),
               factor(c("0-50", "50+", "0-50", "50+"), levels = c("0-50", "50+"), ordered = TRUE))
  

})

