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

#' Clean column data to a class
#' 
#' Use any of these functions to quickly clean columns in your data set. Use \code{clean()} to pick the functions that return the least relative number of \code{NA}s. They \strong{always} return the class from the function name (e.g. \code{clean_Date()} always returns class \code{Date}).
#' @param x data to clean
#' @param true \link[base]{regex} to interpret values as \code{TRUE} (which defaults to \code{\link{regex_true}}), see Details
#' @param false \link[base]{regex} to interpret values as \code{FALSE} (which defaults to \code{\link{regex_false}}), see Details
#' @param na  \link[base]{regex} to force interpret values as \code{NA}, i.e. not as \code{TRUE} or \code{FALSE}
#' @param remove \link[base]{regex} to define the character(s) that should be removed, see Details
#' @param levels new factor levels, may be named with regular expressions to match existing values, see Details
#' @param droplevels logical to indicate whether non-existing factor levels should be dropped
#' @param ordered logical to indicate whether the factor levels should be ordered
#' @param fixed logical to indicate whether regular expressions should be turned off
#' @param trim logical to indicate whether the result should be trimmed with \code{\link{trimws}(x, which = "both")}
#' @param ignore.case logical to indicate whether matching should be case-insensitive
#' @param format a date format that will be passed on to \code{\link{format_datetime}}, see Details
#' @param currency_symbol the currency symbol to use, which will be guessed based on the input and otherwise defaults to the current system locale setting (see \code{\link{Sys.localeconv}})
#' @param ... other parameters passed on to \code{\link{as.Date}} or \code{\link{as.POSIXct}}
#' @details
#' Using \code{clean()} on a vector will guess a cleaning function based on the potential number of \code{NAs} it returns. Using \code{clean()} on a data.frame to apply this guessed cleaning over all columns.
#' 
#' Info about the different functions:
#' 
#' \itemize{
#'   \item{\code{clean_logical()}:\cr}{Use parameters \code{true} and \code{false} to match values using case-insensitive regular expressions (\link[base]{regex}). Unmatched values are considered \code{NA}. At default, values are matched with \code{\link{regex_true}} and \code{\link{regex_false}}. This allows support for values "Yes" and "No" in the following languages: Arabic, Bengali, Chinese (Mandarin), Dutch, English, French, German, Hindi, Indonesian, Japanese, Malay, Portuguese, Russian, Spanish, Telugu, Turkish and Urdu. Use parameter \code{na} to override values as \code{NA} that would else be matched with \code{true} or \code{false}. See Examples.}
#'   \item{\code{clean_factor()}:\cr}{Use parameter \code{levels} to set new factor levels. They can be case-insensitive regular expressions to match existing values of \code{x}. For matching, new values for \code{levels} are internally temporary sorted descending on text length. See Examples.}
#'   \item{\code{clean_numeric()} and \code{clean_character()}:\cr}{Use parameter \code{remove} to match values that must be removed from the input, using regular expressions (\link[base]{regex}). In case of \code{clean_numeric()}, comma's will be read as dots and only the last dot will be kept. Function \code{clean_character()} will keep middle spaces at default. See Examples.}
#'   \item{\code{clean_percentage()}:\cr}{This new class works like \code{clean_numeric()}, but transforms it with \code{\link{as.percentage}}, which will retain the original values, but will print them as percentages. See Examples.} 
#'   \item{\code{clean_currency()}:\cr}{This new class works like \code{clean_numeric()}, but transforms it with \code{\link{as.currency}}. The currency symbol is guessed based on the most traded currencies by value (see Source): the United States dollar, Euro, Japanese yen, Pound sterling, Swiss franc, Renminbi, Swedish krona, Mexican peso, South Korean won, Turkish lira, Russian ruble, Indian rupee and the South African rand. See Examples.}
#'   \item{\code{clean_Date()}:\cr}{Use parameter \code{format} to define a date format, or leave it empty to have the format guessed. Use \code{"Excel"} to read values as Microsoft Excel dates. The \code{format} parameter will be evaluated with \code{\link{format_datetime}}, which means that a format like \code{"d-mmm-yy"} with be translated internally to \code{"\%e-\%b-\%y"} for convenience. See Examples.}
#'   \item{\code{clean_POSIXct()}:\cr}{Use parameter \code{remove} to match values that must be removed from the input, using regular expressions (\link[base]{regex}). The resulting string will be coerced to a date/time element with class \code{POSIXct}, using \code{\link{as.POSIXct}()}. See Examples.}
#' }
#' 
#' The use of invalid regular expressions in any of the above functions will not return an error (like in base R), but will instead interpret the expression as a fixed value and will throw a warning.
#' @rdname clean
#' @return The \code{clean} functions \strong{always} return the class from the function name:
#' \itemize{
#'   \item{\code{clean_logical()}: class \code{logical}}
#'   \item{\code{clean_factor()}: class \code{factor}}
#'   \item{\code{clean_numeric()}: class \code{numeric}}
#'   \item{\code{clean_character()}: class \code{character}}
#'   \item{\code{clean_percentage()}: class \code{percentage}}
#'   \item{\code{clean_currency()}: class \code{currency}}
#'   \item{\code{clean_Date()}: class \code{Date}}
#'   \item{\code{clean_POSIXct()}: classes \code{POSIXct/POSIXt}}
#' }
#' @export
#' @exportMethod clean
#' @source \href{http://www.bis.org/publ/rpfx16fx.pdf}{Triennial Central Bank Survey Foreign exchange turnover in April 2016} (PDF). Bank for International Settlements. 11 December 2016. p. 10.
#' @examples 
#' clean_logical(c("Yes", "No"))   # English
#' clean_logical(c("Oui", "Non"))  # French
#' clean_logical(c("ya", "tidak")) # Indonesian
#' clean_logical(x = c("Positive", "Negative", "Unknown", "Some value"),
#'               true = "pos", false = "neg")
#' 
#' gender_age <- c("male 0-50", "male 50+", "female 0-50", "female 50+")
#' clean_factor(gender_age, c("M", "F"))
#' clean_factor(gender_age, c("Male", "Female"))
#' clean_factor(gender_age, c("0-50", "50+"), ordered = TRUE)
#' 
#' clean_Date("13jul18", "ddmmmyy")
#' clean_Date("12 august 2010")
#' clean_Date("12 06 2012")
#' clean_Date(36526) # Excel date
#' clean_Date("43658")
#' clean_Date("14526", "Excel") # "1939-10-08"
#' 
#' clean_POSIXct("Created log on 2019/04/11 11:23 by user Joe")
#' 
#' clean_numeric("qwerty123456")
#' clean_numeric("Positive (0.143)")
#' clean_numeric("0,143")
#' clean_percentage("PCT: 0.143")
#' 
#' clean_character("qwerty123456")
#' clean_character("Positive (0.143)")
#' 
#' clean_currency(c("Received $ 25", "Received $ 31.40"))
#' clean_currency(c("Jack sent £ 25", "Bill sent £ 31.40"))
#'  
#' clean("12 06 2012")
#' clean(data.frame(dates = "2013-04-02", 
#'                  logicals = c("yes", "no")))
clean <- function(x) {
  UseMethod("clean")
}

#' @exportMethod clean.default
#' @export
#' @noRd
clean.default <- function(x) {
  x_withoutNA <- x[!is.na(x)]
  fns <- c("Date", "numeric", "logical", "character")
  for (i in 1:length(fns)) {
    fn <- get(paste0("clean_", fns[i]), envir = asNamespace("clean"))
    if (all(!is.na(suppressWarnings(suppressMessages(fn(x_withoutNA)))))) {
      message("Cleaning to class '", fns[i], "'")
      return(fn(x_withoutNA))
    }
  }
  warning("no appropriate cleaning function found")
  x
}

#' @exportMethod clean.data.frame
#' @export
#' @rdname clean
clean.data.frame <- function(x) {
  n <- 0
  as.data.frame(lapply(x, function(y) {
    message("[VARIABLE '", colnames(x)[n <<- n + 1], "'] ", appendLF = FALSE)
    clean(y)
  }), stringsAsFactors = FALSE)
}

#' @rdname clean
#' @export
clean_logical <- function(x, true = regex_true(), false = regex_false(), na = NULL, fixed = FALSE, ignore.case = TRUE) {
  # transform x to Latin characters: "sí" will be "si"
  if (identical(true, regex_true()) & identical(false, regex_false())) {
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- gsub("[^a-z,./ 0-9]+", "", x, ignore.case = ignore.case)
  }
  conv <- rep(NA, length(x))
  conv[x %in% c(-1, 1) | grepl_warn_on_error(true, x, ignore.case = ignore.case, fixed = fixed)] <- TRUE
  conv[x == 0 | grepl_warn_on_error(false, x, ignore.case = ignore.case, fixed = fixed)] <- FALSE
  if (!is.null(na)) {
    conv[grepl_warn_on_error(na, x, ignore.case = ignore.case, fixed = fixed)] <- NA
  }
  as.logical(conv)
}

#' @rdname clean
#' @export
clean_factor <- function(x, levels = unique(x), ordered = FALSE, droplevels = FALSE, fixed = FALSE, ignore.case = TRUE) {
  if (!all(levels %in% x)) {
    new_x <- rep(NA_character_, length(x))
    # sort descending on character length
    levels_nchar <- levels[rev(order(nchar(levels)))]
    new_x <- rep(NA_character_, length(x))
    # fill in levels
    for (i in 1:length(levels_nchar)) {
      # first try exact match
      tryCatch(new_x[is.na(new_x) & x == levels_nchar[i]] <- levels_nchar[i], error = function(e) invisible())
      # then regular expressions
      new_x[is.na(new_x) & grepl_warn_on_error(levels_nchar[i], x, ignore.case = ignore.case, fixed = fixed)] <- levels_nchar[i]
    }
    if (!is.null(names(levels))) {
      # override named levels
      x_set_with_name <- logical(length(x))
      for (i in 1:length(levels)) {
        if (names(levels)[i] != "") {
          new_x[grepl_warn_on_error(names(levels)[i], x, ignore.case = ignore.case, fixed = fixed) & x_set_with_name == FALSE] <- levels[i]
          x_set_with_name[grepl_warn_on_error(names(levels)[i], x, ignore.case = ignore.case, fixed = fixed)] <- TRUE
        }
      }
    }
    x <- new_x
  }
  if (length(levels[!levels %in% x]) > 0 & droplevels == FALSE) {
    warning("These factor levels were not found in the data: ", 
            toString(sort(levels[!levels %in% x])), call. = FALSE)
  }
  x <- factor(x = x, levels = levels, ordered = ordered)
  if (droplevels == TRUE) {
    droplevels(x)
  } else {
    x
  }
}

#' @rdname clean
#' @export
clean_numeric <- function(x, remove = "[^0-9.,]", fixed = FALSE) {
  x <- gsub(",", ".", x)
  # remove ending dot/comma
  x <- gsub("[,.]$", "", x)
  # only keep last dot/comma
  reverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
  x <- sub("{{dot}}", ".", 
           gsub(".", "",
                reverse(sub(".", "}}tod{{",
                            reverse(x), 
                            fixed = TRUE)),
                fixed = TRUE), 
           fixed = TRUE)
  as.numeric(gsub_warn_on_error(remove, "", x, ignore.case = TRUE, fixed = fixed))
}

#' @rdname clean
#' @export
clean_character <- function(x, remove = "[^a-z \t\r\n]", fixed = FALSE, ignore.case = TRUE, trim = TRUE) {
  x <- as.character(gsub_warn_on_error(remove, "", x, ignore.case = ignore.case, fixed = fixed))
  if (isTRUE(trim)) {
    trimws(x, which = "both")
  } else {
    x
  }
}

#' @rdname clean
#' @export
clean_currency <- function(x, currency_symbol = NULL, ...) {
  if (isTRUE(any(grepl("[^a-zA-Z]([\u0024]|USD)", x)))) { # Dollar Sign
    currency_symbol <- "USD"
  } else if (isTRUE(any(grepl("[^a-zA-Z]([\u20ac]|EUR)", x)))) { # Euro sign
    currency_symbol <- "EUR"
  } else if (isTRUE(any(grepl("[^a-zA-Z]([\u00a5]|JPY)", x)))) { # Yen sign
    currency_symbol <- "JPY"
  } else if (isTRUE(any(grepl("[^a-zA-Z]([\u00a3]|GBP)", x)))) {
    currency_symbol <- "GBP"
  } else if (isTRUE(any(grepl("[^a-zA-Z](Fr|CHF)", x)))) {
    currency_symbol <- "CHF"
  } else if (isTRUE(any(grepl("[^a-zA-Z]([\u5143]|CNY)", x)))) {
    currency_symbol <- "CNY"
  } else if (isTRUE(any(grepl("[^a-zA-Z](k|SEK)r", x)))) {
    currency_symbol <- "SEK"
  } else if (isTRUE(any(grepl("[^a-zA-Z]([\u20a9]|KRW)", x)))) { # Won sign
    currency_symbol <- "KRW"
  } else if (isTRUE(any(grepl("[^a-zA-Z](TRY)", x)))) { # Lira sign
    currency_symbol <- "TRY"
  } else if (isTRUE(any(grepl("[^a-zA-Z](RUB)", x)))) { # Ruble sign
    currency_symbol <- "RUB"
  } else if (isTRUE(any(grepl("[^a-zA-Z](INR)", x)))) { # Rubee sign
    currency_symbol <- "INR"
  } else if (isTRUE(any(grepl("[^a-zA-Z](ZAR)", x)))) {
    currency_symbol <- "ZAR"
  } else {
    currency_symbol <- Sys.localeconv()["int_curr_symbol"]
  }
  as.currency(clean_numeric(x, ...), currency_symbol = currency_symbol)
}

#' @rdname clean
#' @export
clean_percentage <- function(x, remove = "[^0-9.,]", fixed = FALSE) {
  as.percentage(clean_numeric(x = x, remove = remove, fixed = fixed))
}


#' @rdname clean
#' @export
clean_Date <- function(x, format = NULL, ...) {
  if (!is.null(format)) {
    if (tolower(format) == "excel") {
      return(as.Date(as.numeric(x), origin = "1899-12-30"))
    } else {
      return(as.Date(x = x, format = format_datetime(format), ...))
    }
  }
  
  x.bak <- x
  
  # start guessing
  msg_clean_as <- function(format_set) {
    if (tolower(format_set) == "excel") {
      message("Cleaning dates using Excel format")
    } else {
      message("Cleaning dates using format '", format_set, "' ('", format_datetime(format_set), "')")
    }
  }
  
  x_numeric <- suppressWarnings(as.numeric(x))
  if (all(x_numeric %in% c(as.integer(as.Date("1970-01-01") - as.Date("1899-12-30")):as.integer(Sys.Date() - as.Date("1899-12-30"))), na.rm = TRUE)) {
    # is Excel date
    msg_clean_as("Excel")
    return(as.Date(x_numeric, origin = "1899-12-30"))
  }
  
  
  # replace any non-number/separators ("-", ".", etc.) with space
  x <- trimws(gsub("[^0-9a-z]+", " ", x, ignore.case = TRUE))
  
  # remove 1st, 2nd, 3rd, 4th followed by a space or at the end
  x <- gsub("([0-9])(st|nd|rd|th) ", "\\1", x, ignore.case = TRUE)
  x <- gsub("([0-9])(st|nd|rd|th)$", "\\1", x, ignore.case = TRUE)
  
  # first check if format is like 1-2 digits, text, 2-4 digits (12 August 2010) which is observed a lot
  if (all(grepl("^[0-9]+ [a-z]+ [0-9]+$", x[!is.na(x)], ignore.case = TRUE))) {
    if (all(grepl("^[0-9]{2} [a-z]{3} [0-9]{4}$", x[!is.na(x)], ignore.case = TRUE))) new_format <- "dd mmm yyyy"
    if (all(grepl("^[0-9]{2} [a-z]{3} [0-9]{2}$", x[!is.na(x)], ignore.case = TRUE))) new_format <- "dd mmm yy"
    if (all(grepl("^[0-9]{1} [a-z]{3} [0-9]{4}$", x[!is.na(x)], ignore.case = TRUE))) new_format <- "d mmm yyyy"
    if (all(grepl("^[0-9]{1} [a-z]{3} [0-9]{2}$", x[!is.na(x)], ignore.case = TRUE))) new_format <- "d mmm yy"
    if (all(grepl("^[0-9]{2} [a-z]{4,} [0-9]{4}$", x[!is.na(x)], ignore.case = TRUE))) new_format <- "dd mmmm yyyy"
    if (all(grepl("^[0-9]{2} [a-z]{4,} [0-9]{2}$", x[!is.na(x)], ignore.case = TRUE))) new_format <- "dd mmmm yy"
    if (all(grepl("^[0-9]{1} [a-z]{4,} [0-9]{4}$", x[!is.na(x)], ignore.case = TRUE))) new_format <- "d mmmm yyyy"
    if (all(grepl("^[0-9]{1} [a-z]{4,} [0-9]{2}$", x[!is.na(x)], ignore.case = TRUE))) new_format <- "d mmmm yy"
    msg_clean_as(new_format)
    return(as.Date(as.character(x), format = format_datetime(new_format)))
  }
  
  # now remove spaces too
  x <- gsub(" +", "", x)
  
  # try any dateformat: 1-2 day numbers, 1-3 month numbers, 2/4 year numbers, in any order
  days <- c("d", "dd", "ddd", "dddd")
  months <- c("mm", "mmm", "mmmm")
  years <- c("yyyy", "yy")
  validated_format <- function(x, a, b, c) {
    # strip any non-number ("-", ".", etc.) and remove NAs for testing
    x_withoutNAs <- x[!is.na(x)]
    # create vector with all possible options in the order of a, b, c
    format <- do.call(paste, 
                      expand.grid(a, b, c,
                                  stringsAsFactors = FALSE,
                                  KEEP.OUT.ATTRS = FALSE))
    # sort descending on character length
    format <- format[rev(order(nchar(format)))]
    format_spaced <- format
    # remove spaces from the format too, it was already removed from input
    format <- gsub(" ", "", format)
    for (i in 1:length(format)) {
      validated_dates <- suppressWarnings(as.Date(as.character(x_withoutNAs), 
                                                  format = format_datetime(format[i])))
      if (all(!is.na(validated_dates))
          & all(validated_dates > as.Date("1900-01-01"))
          & (all(grepl("[a-zA-Z]", x)) | all(nchar(x) == nchar(format[i])))) {
        msg_clean_as(format_spaced[i])
        return(format_datetime(format[i]))
      }
    }
    # no valid format found
    return(NULL)
  }
  
  # now try all `3!` (factorial(3) = 6) combinations
  # ymd ydm mdy myd dmy dym
  new_format <- validated_format(x, years, months, days)
  if (!is.null(new_format)) {
    return(as.Date(as.character(x), format = new_format))
  }
  new_format <- validated_format(x, days, months, years)
  if (!is.null(new_format)) {
    return(as.Date(as.character(x), format = new_format))
  }
  new_format <- validated_format(x, months, days, years)
  if (!is.null(new_format)) {
    return(as.Date(as.character(x), format = new_format))
  }
  new_format <- validated_format(x, years, days, months)
  if (!is.null(new_format)) {
    return(as.Date(as.character(x), format = new_format))
  }
  new_format <- validated_format(x, days, years, months)
  if (!is.null(new_format)) {
    return(as.Date(as.character(x), format = new_format))
  }
  new_format <- validated_format(x, months, years, days)
  if (!is.null(new_format)) {
    return(as.Date(as.character(x), format = new_format))
  }
  warning("Date/time format could not be determined automatically, returning NAs", call. = FALSE)
  as.Date(rep(NA, length(x)))
}

#' @rdname clean
#' @export
clean_POSIXct <- function(x, remove = "[^.0-9 :/-]", fixed = FALSE, ...) {
  x <- trimws(gsub_warn_on_error(remove, "", x, ignore.case = TRUE, fixed = fixed))
  x <- gsub("[\\./]", "-", x)
  as.POSIXct(x, ...)
}
