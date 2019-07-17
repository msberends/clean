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

`%like%` <- function(x, pattern) {
  if (length(pattern) > 1) {
    if (length(x) != length(pattern)) {
      pattern <- pattern[1]
      warning("only the first element of argument `pattern` used for `%like%`", 
              call. = TRUE)
    } else {
      res <- vector(length = length(pattern))
      for (i in 1:length(res)) {
        if (is.factor(x[i])) {
          res[i] <- as.integer(x[i]) %in% base::grep(pattern[i], 
                                                     levels(x[i]), ignore.case = TRUE)
        } else {
          res[i] <- base::grepl(pattern[i], x[i], ignore.case = TRUE)
        }
      }
      return(res)
    }
  }
  if (is.factor(x)) {
    as.integer(x) %in% base::grep(pattern, levels(x), ignore.case = TRUE)
  } else {
    base::grepl(pattern, x, ignore.case = TRUE)
  }
}

# throws warning on error, so invalid regex will still run as fixed value
grepl_warn_on_error <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                                fixed = FALSE, useBytes = FALSE) {
  tryCatch(expr = base::grepl(pattern = pattern, x = x, ignore.case = ignore.case, perl = perl,
                              fixed = fixed, useBytes = useBytes),
           error = function(e) {
             warning(paste0(e$message, " - now interpreting as fixed value"), call. = FALSE)
             return(base::grepl(pattern = pattern, x = x, ignore.case = ignore.case,
                                fixed = TRUE, useBytes = useBytes))
           })
}

# works exactly like round(), but rounds `round(44.55, 1)` as 44.6 instead of 44.5
# and adds decimal zeroes until `digits` is reached when force_zero = TRUE
round2 <- function(x, digits = 0, force_zero = TRUE) {
  # https://stackoverflow.com/a/12688836/4575331
  val <- (trunc((abs(x) * 10 ^ digits) + 0.5) / 10 ^ digits) * sign(x)
  if (digits > 0 & force_zero == TRUE) {
    val[val != as.integer(val)] <- paste0(val[val != as.integer(val)],
                                          strrep("0", max(0, digits - nchar(gsub(".*[.](.*)$", "\\1", val[val != as.integer(val)])))))
  }
  val
}

# Coefficient of variation (CV)
cv <- function(x, na.rm = TRUE) {
  stats::sd(x, na.rm = na.rm) / base::abs(base::mean(x, na.rm = na.rm))
}

# Coefficient of dispersion, or coefficient of quartile variation (CQV).
# (Bonett et al., 2006: Confidence interval for a coefficient of quartile variation).
cqv <- function(x, na.rm = TRUE) {
  fives <- stats::fivenum(x, na.rm = na.rm)
  (fives[4] - fives[2]) / (fives[4] + fives[2])
}

# source: scales::number -> scales::percent
percent_scales <- function (x,
                            accuracy = NULL,
                            scale = 100,
                            prefix = "", 
                            suffix = "%", 
                            big.mark = " ",
                            decimal.mark = ".",
                            trim = TRUE, ...) {
  if (length(x) == 0) 
    return(character())
  x <- round(x / (accuracy / scale)) * (accuracy / scale)
  nsmall <- -floor(log10(accuracy))
  nsmall <- min(max(nsmall, 0), 20)
  ret <- format(scale * x, big.mark = big.mark, decimal.mark = decimal.mark, 
                trim = trim, nsmall = nsmall, scientific = FALSE, ...)
  ret <- paste0(prefix, ret, suffix)
  ret[is.infinite(x)] <- as.character(x[is.infinite(x)])
  ret
}

# No export, no Rd
percent <- function(x, round = 1, force_zero = TRUE, decimal.mark = getOption("OutDec"), ...) {
  x <- percent_scales(x = as.double(x),
                      accuracy = 1 / 10 ^ round,
                      decimal.mark = decimal.mark,
                      ...)
  if (force_zero == FALSE) {
    x <- gsub("([.]%|%%)", "%", paste0(gsub("0+%$", "", x), "%"))
  }
  x
}
