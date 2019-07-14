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

#' Frequency table
#'
#' Create a frequency table of a vector with items or a \code{data.frame}. Supports quasiquotation and markdown for reports. Easiest practice is: \code{data \%>\% freq(var)} using the \href{https://magrittr.tidyverse.org/#usage}{tidyverse}.\cr
#' \code{top_freq} can be used to get the top/bottom \emph{n} items of a frequency table, with counts as names. It respects ties.
#' @param x vector of any class or a \code{\link{data.frame}} or \code{\link{table}}
#' @param ... up to nine different columns of \code{x} when \code{x} is a \code{data.frame} or \code{tibble}, to calculate frequencies from - see Examples. Also supports quasiquotion.
#' @param sort.count sort on count, i.e. frequencies. This will be \code{TRUE} at default for everything except when using grouping variables.
#' @param nmax number of row to print. The default, \code{10}, uses \code{\link{getOption}("max.print.freq")}. Use \code{nmax = 0}, \code{nmax = Inf}, \code{nmax = NULL} or \code{nmax = NA} to print all rows.
#' @param na.rm a logical value indicating whether \code{NA} values should be removed from the frequency table. The header (if set) will always print the amount of \code{NA}s.
#' @param row.names a logical value indicating whether row indices should be printed as \code{1:nrow(x)}
#' @param markdown a logical value indicating whether the frequency table should be printed in markdown format. This will print all rows (except when \code{nmax} is defined) and is default behaviour in non-interactive R sessions (like when knitting RMarkdown files).
#' @param digits how many significant digits are to be used for numeric values in the header (not for the items themselves, that depends on \code{\link{getOption}("digits")})
#' @param quote a logical value indicating whether or not strings should be printed with surrounding quotes. Default is to print them only around characters that are actually numeric values.
#' @param header a logical value indicating whether an informative header should be printed
#' @param title text to show above frequency table, at default to tries to coerce from the variables passed to \code{x}
#' @param na a character string that should be used to show empty (\code{NA}) values (only useful when \code{na.rm = FALSE})
#' @param droplevels a logical value indicating whether in factors empty levels should be dropped
#' @param format a character to define the printing format
#' @param sep a character string to separate the terms when selecting multiple columns
#' @inheritParams base::format
#' @param f a frequency table
#' @param n number of top \emph{n} items to return, use -n for the bottom \emph{n} items. It will include more than \code{n} rows if there are ties.
#' @param property property in header to return this value directly
#' @details Frequency tables (or frequency distributions) are summaries of the distribution of values in a sample. With the `freq` function, you can create univariate frequency tables. Multiple variables will be pasted into one variable, so it forces a univariate distribution. This package also has a vignette available to explain the use of this function further, run \code{browseVignettes("clean")} to read it.
#'
#' For numeric values of any class, these additional values will all be calculated with \code{na.rm = TRUE} and shown into the header:
#' \itemize{
#'   \item{Mean, using \code{\link[base]{mean}}}
#'   \item{Standard Deviation, using \code{\link[stats]{sd}}}
#'   \item{Coefficient of Variation (CV), the standard deviation divided by the mean}
#'   \item{Mean Absolute Deviation (MAD), using \code{\link[stats]{mad}}}
#'   \item{Tukey Five-Number Summaries (minimum, Q1, median, Q3, maximum), using \code{\link[stats]{fivenum}}}
#'   \item{Interquartile Range (IQR) calculated as \code{Q3 - Q1} using the Tukey Five-Number Summaries, i.e. \strong{not} using the \code{\link[stats]{quantile}} function}
#'   \item{Coefficient of Quartile Variation (CQV, sometimes called coefficient of dispersion), calculated as \code{(Q3 - Q1) / (Q3 + Q1)} using the Tukey Five-Number Summaries}
#'   \item{Outliers (total count and unique count), using \code{\link[grDevices]{boxplot.stats}}}
#' }
#'
#' For dates and times of any class, these additional values will be calculated with \code{na.rm = TRUE} and shown into the header:
#' \itemize{
#'   \item{Oldest, using \code{\link{min}}}
#'   \item{Newest, using \code{\link{max}}, with difference between newest and oldest}
#' }
#'
#' In factors, all factor levels that are not existing in the input data will be dropped at default.
#'
#' The function \code{top_freq} will include more than \code{n} rows if there are ties.
#' @section Extending the \code{freq()} function:
#' Interested in extending the \code{freq()} function with your own class? Add a method like below to your package, and optionally define some header info by passing a \code{\link{list}} to the \code{.add_header} parameter, like we did with e.g. \code{freq.difftime}:
#' \preformatted{
#' #' @exportMethod freq.difftime
#' #' @noRd
#' freq.difftime <- function(x, ...) {
#'   freq.default(x = x, ...,
#'                .add_header = list(units = attributes(x)$units))
#' }
#' }
#' @keywords summary summarise frequency freq
#' @rdname freq
#' @name freq
#' @return A \code{data.frame} (with an additional class \code{"freq"}) with five columns: \code{item}, \code{count}, \code{percent}, \code{cum_count} and \code{cum_percent}.
#' @export
#' @exportMethod freq
#' @examples
#' \dontrun{
#' # this all gives the same results:
#' freq(df$variable)
#' freq(df[, "variable"])
#' df$variable %>% freq()
#' df[, "variable"] %>% freq()
#' df %>% freq("variable")
#' df %>% freq(variable)
#' }
#' 
#' # Clean data?
#' freq(unclean$gender)
#' 
#' # Clean it and check again:
#' freq(clean_factor(unclean$gender, levels = c("^m" = "Male", "^f" = "Female")))
#' 
#' # dplyr way:
#' # unclean %>% 
#' #   freq(clean_factor(gender,
#' #                     levels = c("^m" = "Male", "^f" = "Female")))
#' #
#' # or:
#' # unclean %>% 
#' #   pull(gender) %>% 
#' #   clean_factor(c("^m" = "Male", "^f" = "Female")) %>% 
#' #   freq()
freq <- function(x, ...) {
  UseMethod("freq")
}

#' @exportMethod freq.default
#' @export
#' @rdname freq
freq.default <- function(x,
                         sort.count = TRUE,
                         nmax = getOption("max.print.freq"),
                         na.rm = TRUE,
                         row.names = TRUE,
                         markdown = !interactive(),
                         digits = 2,
                         quote = NULL,
                         header = TRUE,
                         title = NULL,
                         na = "<NA>",
                         sep = " ",
                         decimal.mark = getOption("OutDec"),
                         big.mark = "",
                         ...) {

  format <- list(...)$format

  NAs <- x[is.na(x)]
  if (na.rm == TRUE) {
    x_class <- class(x)
    x <- x[!x %in% NAs]
    class(x) <- x_class
  }
  
  markdown_line <- ""
  if (markdown == TRUE) {
    markdown_line <- "  " # ending with two spaces results in newline
  }
  
  # set header
  header_list <- list(class = class(x),
                      mode = mode(x))
  header_list$length <- length(x)
  if (!is.null(levels(x))) {
    header_list$levels <- levels(x)
    header_list$ordered <- is.ordered(x)
    # drop levels of non-existing factor values,
    # since dplyr >= 0.8.0 does not do this anymore in group_by
    x <- droplevels(x)
  }
  header_list$na_length <- length(NAs)
  header_list$unique <- length(unique(x))
  # add class-specific properties set by freq.class() functions
  header_list <- c(header_list, list(...)$.add_header)
  
  # set nmax
  nmax.set <- !missing(nmax)
  if (identical(nmax, -1)) {
    nmax.set <- FALSE
    nmax <- base::getOption("max.print.freq")
  }
  if (!nmax.set & is.null(nmax) & is.null(base::getOption("max.print.freq", default = NULL))) {
    # default for max print setting
    nmax <- 10
  } else if (is.null(nmax)) {
    nmax <- length(x)
  }
  if (nmax %in% c(0, Inf, NA, NULL)) {
    nmax <- length(x)
  }
  
  # set alignment for Item column
  x_align <- "l"
  if (any(class(x) %in% c("double", "integer", "numeric", "raw", "single"))) {
    x_align <- "r"
  }
  column_align <- c(x_align, "r", "r", "r", "r")
  
  if (!is.null(format)) {
    is.Date <- function(x) inherits(x, c("Date", "POSIXct"))
    x <- format(x, format = ifelse(is.Date(x), format_datetime(format), format))
    quote <- FALSE
  }
  
  # create the data.frame
  df <- base::as.data.frame(base::table(x), stringsAsFactors = FALSE, )
  colnames(df) <- c("item", "count")
  # above is same as (but I want to keep it non-dplyr-dependent):
  # df <- tibble(item = x) %>%
  #   group_by(item) %>%
  #   summarise(count = n())
  
  # reset original class
  if (length(NAs) > 0 & !isTRUE(na.rm)) {
    original <- c(sort(unique(x)), NA)
  } else {
    original <- sort(unique(x))
  }
  tryCatch(df$item <- original, error = function(e) invisible())

  # sort according to setting
  if (sort.count == TRUE) {
    df <- df[order(-df$count),] # descending
  } else {
    df <- df[order(tolower(df$item)),] # ascending
  }
  rownames(df) <- NULL

  # remove escape char
  # see https://en.wikipedia.org/wiki/Escape_character#ASCII_escape_character
  if ("\033" %in% x) {
    df$item <- gsub("\033", " ", df$item, fixed = TRUE)
  }

  if (is.null(quote)) {
    if (!is.numeric(x) & all(x %like% "^[0-9]+$", na.rm = TRUE)) {
      quote <- TRUE
    } else {
      quote <- FALSE
    }
  }
  if (quote == TRUE) {
    df$item <- paste0('"', df$item, '"')
  }

  df$percent <- df$count / base::sum(df$count, na.rm = TRUE)
  df$cum_count <- base::cumsum(df$count)
  df$cum_percent <- df$cum_count / base::sum(df$count, na.rm = TRUE)
  
  if (markdown == TRUE) {
    tbl_format <- "markdown"
  } else {
    tbl_format <- "pandoc"
  }
  
  column_names <- c("Item", "Count", "Percent", "Cum. Count", "Cum. Percent")
  
  structure(.Data = df,
            class = unique(c("freq", class(df))),
            header = header_list, # header info
            opt = list(header = header, # TRUE/FALSE
                       row_names = row.names,
                       column_names = column_names,
                       column_align = column_align,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark,
                       tbl_format = tbl_format,
                       na = na,
                       digits = digits,
                       nmax = nmax,
                       nmax.set = nmax.set))
}

#' @exportMethod freq.default
#' @export
#' @rdname freq
freq.factor <- function(x, ..., droplevels = FALSE) {
  if (isFALSE(droplevels)) {
    freq.default(x, ...)
  } else {
    freq.default(droplevels(x), ...)
  }
}

#' @exportMethod freq.list
#' @noRd
freq.list <- function(x, ...) {
  freq(as.data.frame(x, stringsAsFactors = FALSE), ...)
}

#' @exportMethod freq.matrix
#' @export
#' @rdname freq
freq.matrix <- function(x, ..., quote = FALSE) {
  freq(as.data.frame(x, stringsAsFactors = FALSE), ..., quote = quote)
}

#' @exportMethod freq.table
#' @export
#' @rdname freq
freq.table <- function(x, ..., sep = " ") {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  # now this DF contains 3 columns: the 2 vars and a Freq column
  # paste the first 2 cols and repeat them Freq times:
  x <- rep(x = do.call(paste, c(x[colnames(x)[1:2]], sep = sep)),
           times = x$Freq)
  freq(x, ...)
}

#' @exportMethod freq.data.frame
#' @importFrom rlang enquos eval_tidy
#' @noRd
freq.data.frame <- function(x,
                            ...,
                            sort.count = TRUE,
                            nmax = getOption("max.print.freq", -1),
                            na.rm = TRUE,
                            row.names = TRUE,
                            markdown = !interactive(),
                            digits = 2,
                            quote = NULL,
                            header = TRUE,
                            title = NULL,
                            na = "<NA>",
                            sep = " ",
                            decimal.mark = getOption("OutDec"),
                            big.mark = ifelse(decimal.mark != ",", ",", ".")) {
  # unset data.table, tbl_df, etc.
  # also removes groups made by dplyr::group_by
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  user_exprs <- enquos(...)
  
  if (length(user_exprs) > 0) {
    new_list <- list(0)
    for (i in 1:length(user_exprs)) {
      new_list[[i]] <- tryCatch(eval_tidy(user_exprs[[i]], data = x),
                                error = function(e) stop(e$message, call. = FALSE))
      if (length(new_list[[i]]) == 1) {
        if (i == 1) {
          # only for first item:
          if (is.character(new_list[[i]]) & new_list[[i]] %in% colnames(x)) {
            # support: df %>% freq("mycol")
            new_list[[i]] <- x[, new_list[[i]]]
          }
        } else {
          # remove item - it's a parameter like `nmax`
          new_list[[i]] <- NULL
        }
      }
    }
    x <- as.data.frame(new_list, stringsAsFactors = FALSE)
  }
  
  if (NCOL(x) > 9) {
    stop("maximum of 9 columns allowed", call. = FALSE)
  } else if (NCOL(x) > 1) {
    # paste all columns together
    x <- do.call(paste, c(x[colnames(x)], sep = sep))
  } else {
    x <- x[, 1]
  }
  
  freq(x = x,
       sort.count = sort.count,
       nmax = nmax,
       na.rm = na.rm,
       row.names = row.names,
       markdown = markdown,
       digits = digits,
       quote = quote,
       header = header,
       title = title,
       na = na,
       sep = sep,
       decimal.mark = decimal.mark,
       big.mark = big.mark)
}

#' @exportMethod freq.character
#' @export
#' @noRd
freq.character <- function(x, ...) {
  freq.default(x = x, ..., 
               .add_header = list(shortest = min(nchar(x), na.rm = TRUE),
                                  longest = max(nchar(x), na.rm = TRUE)))
}

#' @exportMethod freq.numeric
#' @export
#' @rdname freq
freq.numeric <- function(x, ..., digits = 2) {
  Tukey_five = stats::fivenum(x, na.rm = TRUE)
  Outliers <- grDevices::boxplot.stats(x)
  
  # create a header like:
  # Mean:      71.06
  # SD:        14.12 (CV: 0.20, MAD: 13.34)
  # Five-Num:  14 | 63 | 74 | 82 | 97 (IQR: 19, CQV: 0.13)
  # Outliers:  32 (unique count: 12)
  
  freq.default(x = x, ..., 
               .add_header = list(mean = round2(base::mean(x, na.rm = TRUE), digits = digits),
                                  SD = paste0(round2(stats::sd(x, na.rm = TRUE), digits = digits),
                                              " (CV: ", round2(cv(x, na.rm = TRUE), digits = digits),
                                              ", MAD: ", round2(stats::mad(x, na.rm = TRUE), digits = digits),
                                              ")"),
                                  `Five-Num` = paste0(paste(trimws(round2(Tukey_five, digits = digits)), collapse = " | "),
                                                      " (IQR: ", round2(Tukey_five[4] - Tukey_five[2], digits = digits), 
                                                      ", CQV: ", round2(cqv(x, na.rm = TRUE), digits = digits), 
                                                      ")"),
                                  outliers = paste0(length(Outliers$out),
                                                    " (", percent(length(Outliers$out) / length(x[!is.na(x)]), 
                                                                  force_zero = TRUE), ")")))
}
#' @exportMethod freq.double
#' @export
#' @noRd
freq.double <- function(x, ...) {
  freq.numeric(x, ...)
}
#' @exportMethod freq.integer
#' @export
#' @noRd
freq.integer <- function(x, ...) {
  freq.numeric(x, ...)
}
#' @exportMethod freq.Date
#' @export
#' @rdname freq
freq.Date <- function(x, ..., format = "yyyy-mm-dd") {
  freq.default(x = x, ..., 
               format = format,
               .add_header = list(oldest = format(min(x, na.rm = TRUE), 
                                                  format = format_datetime(format)),
                                  newest = paste0(
                                    format(max(x, na.rm = TRUE), 
                                           format = format_datetime(format)),
                                    " (+", as.integer(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
                                    " days)")))
}
#' @exportMethod freq.POSIXct
#' @noRd
freq.POSIXct <- function(x, ...) {
  freq.Date(x, ...)
}

#' @exportMethod freq.difftime
#' @noRd
freq.difftime <- function(x, ...) {
  freq.default(x = x, ...,
               .add_header = list(units = attributes(x)$units))
}
#' @exportMethod freq.hms
#' @export
#' @rdname freq
freq.hms <- function(x, ..., format = "HH:MM:SS") {
  freq.default(x = as.POSIXlt(x), ...,
               format = format,
               .add_header = list(earliest = format(min(x, na.rm = TRUE), 
                                                    format = format_datetime(format)),
                                  latest = format(max(x, na.rm = TRUE), 
                                                  format = format_datetime(format))))
}

#' @export
#' @rdname freq
is.freq <- function(f) {
  any(class(f) == "freq")
}

#' @importFrom crayon silver green red
format_header <- function(x, markdown = FALSE, decimal.mark = ".", big.mark = ",", digits = 2) {
  newline <-"\n"
  if (markdown == TRUE) {
    newline <- "  \n"
    # no colours in markdown
    silver <- function(x) x
    green <- function(x) x
    red <- function(x) x
  }

  header <- header(x)
  has_length <- header$length > 0

  # FORMATTING
  # class and mode
  if (!header$mode %in% header$class) {
    header$class <- paste0(paste(rev(header$class), collapse = " > "), silver(paste0(" (", header$mode, ")")))
  } else {
    header$class <- paste(rev(header$class), collapse = " > ")
  }
  header <- header[names(header) != "mode"]
  
  # levels
  if (!is.null(header$levels)) {
    if (header$ordered == TRUE) {
      levels_text <- paste0(header$levels, collapse = " < ")
    } else {
      levels_text <- paste0(header$levels, collapse = ", ")
    }
    if (nchar(levels_text) > (options()$width / 2)) {
      # levels text wider than half the console
      levels_text <- paste0(substr(levels_text, 1, 70 - 3), "...")
      if (nchar(gsub("[^`]", "", levels_text)) %% 2 == 1) {
        # odd number of backticks, should be even
        levels_text <- paste0(levels_text, "`")
      }
    }
    header$levels <- paste0(length(header$levels), ": ", levels_text)
    header <- header[names(header) != "ordered"]
  }
  # length and NAs
  if (has_length == TRUE) {
    na_txt <- paste0(format(header$na_length, decimal.mark = decimal.mark, big.mark = big.mark), " = ",
                     sub("NaN", "0", percent(header$na_length / header$length, force_zero = TRUE, round = digits, decimal.mark = decimal.mark), fixed = TRUE))
    if (!na_txt %like% "^0 =") {
      na_txt <- red(na_txt)
    } else {
      na_txt <- green(na_txt)
    }
    na_txt <- paste0("(of which NA: ", na_txt, ")")
  } else {
    na_txt <- ""
  }
  header$length <- paste(format(header$length, decimal.mark = decimal.mark, big.mark = big.mark),
                         na_txt)
  header <- header[names(header) != "na_length"]

  # format all numeric values
  header <- lapply(header, function(x) {
    if (is.numeric(x)) {
      if (any(x < 1000, na.rm = TRUE)) {
        format(round2(x, digits = digits), decimal.mark = decimal.mark, big.mark = big.mark)
      } else {
        format(x, digits = digits, decimal.mark = decimal.mark, big.mark = big.mark)
      }
    } else {
      x
    }
  })

  # header names
  header_names <- paste0(names(header), ":  ")
  # capitalise first character
  header_names <- gsub("^(.)", "\\U\\1", header_names, perl = TRUE)
  # make all header captions equal size
  header_names <- gsub("\\s", " ", format(header_names,
                                          width = max(nchar(header_names),
                                                      na.rm = TRUE)))
  header <- paste0(header_names, header)
  header <- paste(header, collapse = newline)
  # add newline after 'Unique'
  gsub("(.*Unique.*\\n)(.*?)", paste0("\\1", newline, "\\2"), header)
}

#' @rdname freq
#' @export
top_freq <- function(f, n) {
  if (!is.freq(f)) {
    stop("`top_freq` can only be applied to frequency tables", call. = FALSE)
  }
  if (!is.numeric(n) | length(n) != 1L) {
    stop("For `top_freq`, 'n' must be a number of length 1", call. = FALSE)
  }
  count_at_place_n <- f[n, "count"]
  top <- f[which(f$count >= count_at_place_n), ]
  vect <- top[, "item"]
  names(vect) <- top[, "count"]
  if (length(vect) > abs(n)) {
    message("top_freq: selecting ", length(vect), " items instead of ", abs(n), ", because of ties")
  }
  vect
}

#' @rdname freq
#' @export
header <- function(f, property = NULL) {
  if (!is.freq(f)) {
    stop("`header` can only be applied to frequency tables", call. = FALSE)
  }
  if (is.null(property)) {
    attributes(f)$header
  } else {
    a <- attributes(f)$header
    if (any(property %in% names(f))) {
      a[names(a) %in% property]
    }
  }
}

#' @rdname freq
#' @exportMethod print.freq
#' @importFrom knitr kable
#' @importFrom crayon bold silver
#' @export
print.freq <- function(x,
                       nmax = getOption("max.print.freq", default = 10),
                       markdown = !interactive(),
                       header = TRUE,
                       decimal.mark = getOption("OutDec"),
                       big.mark = ifelse(decimal.mark != ",", ",", "."),
                       ...) {

  opt <- attr(x, "opt")
  if (is.null(opt)) {
    # selection of frequency table, return original class
    class(x) <- class(x)[!class(x) %in% c("freq", "frequency_tbl")]
    print(x)
    return(invisible())
  }

  opt$header_txt <- header(x)
  if (is.null(opt$nmax)) {
    opt$nmax <- 0
  }
  if (is.null(opt$tbl_format)) {
    opt$tbl_format <- "pandoc"
  }

  dots <- list(...)
  if ("markdown" %in% names(dots)) {
    if (dots$markdown == TRUE) {
      opt$tbl_format <- "markdown"
    } else {
      opt$tbl_format <- "pandoc"
    }
  }
  if (!missing(markdown)) {
    if (markdown == TRUE) {
      opt$tbl_format <- "markdown"
    } else {
      opt$tbl_format <- "pandoc"
    }
  }
  
  if (!missing(nmax) | is.null(opt$nmax)) {
    opt$nmax <- nmax
    opt$nmax.set <- TRUE
  }
  if (isTRUE(opt$nmax %in% c(0, Inf, NA, NULL))) {
    opt$nmax <- NROW(x)
    opt$nmax.set <- FALSE
  } else if (isTRUE(opt$nmax >= NROW(x))) {
    opt$nmax.set <- FALSE
  }

  if (!missing(decimal.mark) | is.null(opt$decimal.mark)) {
    opt$decimal.mark <- decimal.mark
  }
  if (!missing(big.mark) | is.null(opt$big.mark)) {
    opt$big.mark <- big.mark
  }
  if (!missing(header)) {
    opt$header <- header
  }

  title <- "Frequency table"
  if (isTRUE(opt$tbl_format == "pandoc")) {
    title <- bold(title)
  } else if (isTRUE(opt$tbl_format == "markdown")) {
    title <- paste0("\n\n**", title, "**  ") # two space for newline
  }

  cat(title, "\n\n")

  if (NROW(x) == 0 | isTRUE(all(is.na(x$item)))) {
    cat("No observations")
    if (isTRUE(all(is.na(x$item) | identical(x$item, "<NA>") | identical(x$item, "(NA)")))) {
      cat(" - all values are missing (NA)")
    }
    cat(".\n")
    if (opt$tbl_format == "markdown") {
      cat("\n")
    }
    return(invisible())
  }

  if (isTRUE(opt$header == TRUE)) {
    if (!is.null(opt$header_txt)) {
      if (is.null(opt$digits)) {
        opt$digits <- 2
      }
      cat(format_header(x, digits = opt$digits, markdown = (opt$tbl_format == "markdown"),
                        decimal.mark = decimal.mark, big.mark = big.mark))
    }
  }

  # save old NA setting for kable
  opt.old <- options()$knitr.kable.NA
  if (is.null(opt$na)) {
    opt$na <- "<NA>"
  }
  if (isTRUE(opt$tbl_format == "markdown")) {
    # no HTML tags
    opt$na <- gsub("<", "(", opt$na, fixed = TRUE)
    opt$na <- gsub(">", ")", opt$na, fixed = TRUE)
  }
  options(knitr.kable.NA = opt$na)

  x.rows <- nrow(x)
  x.unprinted <- base::sum(x[(opt$nmax + 1):nrow(x), "count"], na.rm = TRUE)
  x.printed <- base::sum(x$count) - x.unprinted

  if (nrow(x) > opt$nmax & isTRUE(opt$tbl_format != "markdown")) {

    if (opt$nmax.set == TRUE) {
      nmax <- opt$nmax
    } else {
      nmax <- getOption("max.print.freq", default = 10)
    }

    x <- x[1:nmax,]

    if (opt$nmax.set == TRUE) {
      footer <- paste("[ reached `nmax = ", opt$nmax, "`", sep = "")
    } else {
      footer <- '[ reached getOption("max.print.freq")'
    }
    footer <- paste(footer,
                    " -- omitted ",
                    format(x.rows - opt$nmax, big.mark = opt$big.mark, decimal.mark = opt$decimal.mark),
                    " entries, n = ",
                    format(x.unprinted, big.mark = opt$big.mark, decimal.mark = opt$decimal.mark),
                    " (",
                    percent(x.unprinted / (x.unprinted + x.printed), force_zero = TRUE, decimal.mark = opt$decimal.mark),
                    ") ]\n", sep = "")
    if (opt$tbl_format == "pandoc") {
      footer <- silver(footer) # only silver in regular printing
    }
  } else if (opt$tbl_format == "markdown") {
    if (opt$nmax.set == TRUE) {
      x <- x[1:opt$nmax,]
      footer <- paste("\n(omitted ",
                      format(x.rows - opt$nmax, big.mark = opt$big.mark, decimal.mark = opt$decimal.mark),
                      " entries, n = ",
                      format(x.unprinted, big.mark = opt$big.mark, decimal.mark = opt$decimal.mark),
                      " [",
                      percent(x.unprinted / (x.unprinted + x.printed), force_zero = TRUE, decimal.mark = opt$decimal.mark),
                      "])\n", sep = "")
    } else {
      footer <- NULL
    }
  } else {
    footer <- NULL
  }

  if ("item" %in% colnames(x)) {
    if (any(class(x$item) %in% c("double", "integer", "numeric", "raw", "single"))) {
      x$item <- format(x$item, decimal.mark = opt$decimal.mark, big.mark = opt$big.mark)
    }
  } else {
    opt$column_names <- opt$column_names[!opt$column_names == "Item"]
  }

  all_unique <- FALSE
  if ("count" %in% colnames(x)) {
    if (all(x$count == 1)) {
      all_unique <- TRUE
    }
    x$count <- format(x$count, decimal.mark = opt$decimal.mark, big.mark = opt$big.mark)
  } else {
    opt$column_names <- opt$column_names[!opt$column_names == "Count"]
  }
  if ("percent" %in% colnames(x)) {
    x$percent <- percent(x$percent, force_zero = TRUE, decimal.mark = opt$decimal.mark)
  } else {
    opt$column_names <- opt$column_names[!opt$column_names == "Percent"]
  }
  if ("cum_count" %in% colnames(x)) {
    x$cum_count <- format(x$cum_count, decimal.mark = opt$decimal.mark, big.mark = opt$big.mark)
  } else {
    opt$column_names <- opt$column_names[!opt$column_names == "Cum. Count"]
  }
  if ("cum_percent" %in% colnames(x)) {
    x$cum_percent <- percent(x$cum_percent, force_zero = TRUE, decimal.mark = opt$decimal.mark)
  } else {
    opt$column_names <- opt$column_names[!opt$column_names == "Cum. Percent"]
  }

  if (opt$tbl_format == "markdown") {
    cat("\n")
  }

  if (is.null(opt$row_names)) {
    opt$row_names <- TRUE
  }
  if (is.null(opt$column_names)) {
    opt$column_names <- colnames(x)
  }

  print(
    knitr::kable(x,
                 format = opt$tbl_format,
                 row.names = opt$row_names,
                 col.names = opt$column_names,
                 align = opt$column_align,
                 padding = 1)
  )

  if (!is.null(footer)) {
    cat(footer)
  }

  if (opt$tbl_format == "markdown") {
    cat("\n\n")
  } else {
    cat("\n")
  }

  if (all_unique == TRUE) {
    message("NOTE: All observations are unique.")
  }

  # reset old kable setting
  options(knitr.kable.NA = opt.old)
  return(invisible())

}

#' @noRd
#' @exportMethod as.data.frame.freq
#' @export
as.data.frame.freq <- function(x, ...) {
  attr(x, "opt") <- NULL
  as.data.frame.data.frame(x, ...)
}

#' @noRd
#' @exportMethod hist.freq
#' @export
#' @importFrom graphics hist
hist.freq <- function(x, breaks = "Sturges", main = NULL, xlab = NULL, ...) {
  opt <- attr(x, "opt")
  if (!class(x$item) %in% c("numeric", "double", "integer", "Date")) {
    stop("`x` must be numeric or Date.", call. = FALSE)
  }
  if (!is.null(opt$vars)) {
    title <- opt$vars
  } else if (!is.null(opt$data)) {
    title <- opt$data
  } else {
    title <- "frequency table"
  }
  if (class(x$item) == "Date") {
    x <- as.Date(as.vector(x), origin = "1970-01-01")
  } else {
    x <- as.vector(x)
  }
  if (is.null(main)) {
    main <- paste("Histogram of", title)
  }
  if (is.null(xlab)) {
    xlab <- title
  }
  hist(x, main = main, xlab = xlab, breaks = breaks, ...)
}

#' @noRd
#' @exportMethod boxplot.freq
#' @export
#' @importFrom graphics boxplot
boxplot.freq <- function(x, main = NULL, xlab = NULL, ...) {
  opt <- attr(x, "opt")
  if (!class(x$item) %in% c("numeric", "double", "integer", "Date")) {
    stop("`x` must be numeric or Date.", call. = FALSE)
  }
  if (!is.null(opt$vars)) {
    title <- opt$vars
  } else if (!is.null(opt$data)) {
    title <- opt$data
  } else {
    title <- "frequency table"
  }
  if (class(x$item) == "Date") {
    x <- as.Date(as.vector(x), origin = "1970-01-01")
  } else {
    x <- as.vector(x)
  }
  if (is.null(main)) {
    main <- paste("Boxplot of", title)
  }
  if (is.null(xlab)) {
    xlab <- title
  }
  
  boxplot(x, main = main, xlab = xlab, ...)
}

#' @noRd
#' @exportMethod plot.freq
#' @importFrom graphics plot
#' @export
plot.freq <- function(x, y, ...) {
  opt <- attr(x, "opt")
  if (!is.null(opt$vars)) {
    title <- opt$vars
  } else {
    title <- ""
  }
  plot(x = x$item, y = x$count, ylab = "Count", xlab = title, ...)
}

#' @noRd
#' @exportMethod as.vector.freq
as.vector.freq <- function(x, mode = "any") {
  as.vector(rep(x$item, x$count), mode = mode)
}

#' @noRd
#' @exportMethod format.freq
format.freq <- function(x, digits = 1, ...) {
  opt <- attr(x, "opt")
  if (opt$nmax.set == TRUE) {
    nmax <- opt$nmax
  } else {
    nmax <- getOption("max.print.freq", default = 10)
  }

  x <- x[1:nmax,]
  x$percent <- percent(x$percent, round = digits, force_zero = TRUE)
  x$cum_percent <- percent(x$cum_percent, round = digits, force_zero = TRUE)
  base::format.data.frame(x, ...)
}

