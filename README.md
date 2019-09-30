% clean

# `clean`: Fast and Easy Data Cleaning

**Website of this package: https://msberends.github.io/clean**

[![CRAN_Badge](https://www.r-pkg.org/badges/version/clean)](https://CRAN.R-project.org/package=clean)

The R package for **cleaning and checking data columns** in a fast and easy way. Relying on very few dependencies, it provides **smart guessing**, but with user options to override anything if needed.

It also provides two new data types that are not available in base R: [`currency`](./reference/currency.html) and [`percentage`](./reference/percentage.html).

----

Contents:

* [Why this package](#why-this-package)
* [How it works](#how-it-works)
  * [Cleaning](#cleaning)
  * [Checking](#checking)
* [Speed](#speed)
* [Invalid regular expressions](#invalid-regular-expressions)

----

## Why this package
As a data scientist, I'm often served with data that is not clean, not tidy and consquently not ready for analysis at all. For tidying data, there's of course the `tidyverse` (https://www.tidyverse.org), which lets you manipulate data in any way you can think of. But for *cleaning*, I think our community was still lacking a neat solution that makes data cleaning fast and easy with functions that kind of 'think on their own' to do that.

If the CRAN button at the top of this page is green, install the package with:

```r
install.packages("clean")
```

Otherwise, or if you are looking for the latest stable development version, install the package with:
```r
install.packages("devtools") # if you haven't already
devtools::install_github("msberends/clean")
```

## How it works

This package provides two types of functions: **cleaning** and **checking**.

### Cleaning

Use `clean()` to clean data. It guesses what kind of data class would best fit your input data. It calls any of the following functions, that can also be used independently. They **always** return the class from the function name (e.g. `clean_Date()` always returns class `Date`).
  
* `clean_logical()` for values `TRUE`/`FALSE`. You only define what should be `TRUE` or `FALSE` and it handles the rest for you. At default, it supports "Yes" and "No" in the following languages: Arabic, Bengali, Chinese (Mandarin), Dutch, English, French, German, Hindi, Indonesian, Japanese, Malay, Portuguese, Russian, Spanish, Telugu, Turkish and Urdu. This covers at least two-third of the world population (Ulrich Ammon *et al.*, University of Düsseldorf).

  ```r
  # English
  clean_logical(c("Yes", "No", "Invalid", "Unknown"))
  #> [1]  TRUE FALSE    NA    NA
  
  # French
  clean_logical(c("Oui, c'est ca", "Non, pas encore")) 
  #> [1]  TRUE FALSE
    
  # Indonesian
  clean_logical(c("ya :)", "tidak :("))
  #> [1]  TRUE FALSE
  ```
  
  If you define the `true` and `false` parameters yourself, they will be interpreted as regular expressions:

  ```r
  clean_logical(x = c("Positive", "Negative", "Unknown", "Unknown"),
                true = "pos",
                false = "neg")
  #> [1]  TRUE FALSE    NA    NA
  
  clean_logical(x = c("Probable", "Not probable"),
                true = ".*",
                false = "not")
  #> [1]  TRUE FALSE
  ```
  
* `clean_factor()` for setting and redefining a `factor`. You can use regular expressions to match values in your data to set new factor levels.

  ```r
  gender_age <- c("male 0-50", "male 50+", "female 0-50", "female 50+")
  gender_age
  #> [1] "male 0-50"   "male 50+"    "female 0-50" "female 50+"
  
  clean_factor(gender_age, levels = c("M", "F"))
  #> [1] M M F F
  #> Levels: M F
  
  clean_factor(gender_age, levels = c("Male", "Female"))
  #> [1] Male   Male   Female Female
  #> Levels: Male Female
  
  clean_factor(gender_age, levels = c("0-50", "50+"), ordered = TRUE)
  #> [1] 0-50 50+  0-50 50+ 
  #> Levels: 0-50 < 50+
  ```
  
  You can also name your levels to let them match your values. They support regular expressions too:
  
  ```r
  clean_factor(gender_age, levels = c("female" = "Group A", 
                                      "male 50+" = "Group B",
                                      ".*" = "Other"))
  #> [1] Other   Group B Group A Group A
  #> Levels: Group A Group B Other
  ```
    
* `clean_Date()` for any type of dates. This could be dates imported from Excel, or any combination of days, months and years. For convenience, the `format` parameter understands the date format language of Excel (like `d-mmm-yyyy`) and transforms it internally to the human-unreadable POSIX standard that R understands (`%e-%b-%Y`):

  ```r
  clean_Date("13jul18", "ddmmmyy")
  #> [1] "2018-07-13"
  
  clean_Date("12 06 2012")
  #> Cleaning dates using format 'dd mm yyyy' ('%d %m %Y')
  #> [1] "2012-06-12"
  
  clean_Date("14 august 2010")
  #> Cleaning dates using format 'dd mmmm yyyy' ('%d %B %Y')
  #> [1] "2010-08-14"
  
  clean_Date(38071)
  #> Cleaning dates using Excel format
  #> [1] "2004-03-25"
  ```
  
  The function to transform `d-mmm-yyyy` to `%e-%b-%Y` is available as `format_datetime()` to users. This makes it possible to use it in other date functions too:
  
  ```r
  as.Date("12-13-14", format = format_datetime("mm-yy-dd"))
  #> [1] "2013-12-14"
  ```
  
* `clean_POSIXct()` to remove all non-date/time characters and transform to a date/time element. It automatically adds the systems timezone, which can be changed by the user:

  ```r
  a <- clean_POSIXct("Created log on 2019/04/11 11:23 by user Joe")
  a
  #> "2019-04-11 11:23:00 CEST"
  
  b <- clean_POSIXct("Log am 2019.04.11 11:23 erstellt", tz = "US/Michigan")
  b
  #> "2019-04-11 11:23:00 EDT"
  
  difftime(a, b)
  #> Time difference of -6 hours
  ```
  
* `clean_numeric()` to remove all non-numbers from cluttered input text. It understands usage of dots and comma's in different languages:
  
  ```r
  clean_numeric(c("$ 12,345.67",
                  "€ 12.345,67",
                  "12,345.67",
                  "12345,67"))
  #> [1] 12345.67 12345.67 12345.67 12345.67
  
  clean_numeric("qwerty123456")
  #> [1] 123456
  
  clean_numeric("Positive (0.143)")
  #> [1] 0.143
  ```
  
* `clean_character()` to remove all obvious non-characters from cluttered input text:
  
  ```r
  clean_character("qwerty123456")
  #> [1] "qwerty"
  
  clean_character("Positive (0.143)")
  #> [1] "Positive"
  ```
  
  You can define yourself what should be removed using the `remove` argument, with regular expressions:
  
  ```r
  clean_character(x = c("Model: Pro A1          ",
                        "Model specified: Pro A1",
                        "       Pro A1          "), 
                  remove = "^.*:")
  #> [1] "Pro A1" "Pro A1" "Pro A1"
  ```
  
* `clean_percentage()` to use the new `percentage` class that comes with this package. It prints numeric values as percentages using `as.percentage()`:
  
  ```r
  as.percentage(c(0.25, 2.5, 0.025))
  #> [1]  25.0% 250.0%   2.5%
  
  clean_percentage("PCT: 0.143")
  #> [1] 14.3%
  ```
  
* `clean_currency()` to use the new `currency` class that comes with this package. It transforms the input with `clean_numeric()` first, after which it will be transformed with `as.currency()`, guessing the currency symbol based on your system locale:

  ```r
  clean_currency(c("Jack sent £ 25", "Bill sent £ 31.40"))
  #> [1] `GBP 25.00` `GBP 31.40`
  
  received <- clean_currency(c("Received $25", "Received $31.40"))
  received
  #> [1] `USD 25.00` `USD 31.40`
  
  sum(received)
  #> [1] `USD 56.40`
  
  format(sum(received), 
         currency_symbol = "€", decimal.mark = ",")
  #> [1] "€ 56,40"
  ```
  
  This new class also comes with support for printing in `tibble`s, used by the [`tidyverse`](https://www.tidyverse.org):
  
  ```r
  library(tibble)
  tibble(money = clean_currency(c("Jack sent £ 25", "Bill sent £ 31.40")))
  #> # A tibble: 2 x 1
  #>         money
  #>   <crncy/GBP>
  #> 1       25.00
  #> 2       31.40
  ```

  
### Checking

The easiest and most comprehensive way to check the data of a column/variable is to create frequency tables. Use `freq()` to do this. It supports a lot of different classes (types of data) and is even extendible by other packages.

```r
freq(unclean$gender)
#> Frequency table 
#> 
#> Class:     character
#> Length:    500 (of which NA: 0 = 0.00%)
#> Unique:    5
#> 
#> Shortest:  1
#> Longest:   6
#> 
#>      Item      Count   Percent   Cum. Count   Cum. Percent
#> ---  -------  ------  --------  -----------  -------------
#> 1    male        240     48.0%          240          48.0%
#> 2    female      220     44.0%          460          92.0%
#> 3    man          22      4.4%          482          96.4%
#> 4    m            15      3.0%          497          99.4%
#> 5    F             3      0.6%          500         100.0%
```

Clean it and check again:

```r
freq(clean_factor(unclean$gender, 
                  levels = c("^m" = "Male", "^f" = "Female")))
#> Frequency table 
#> 
#> Class:   factor (numeric)
#> Length:  500 (of which NA: 0 = 0.00%)
#> Levels:  2: Male, Female
#> Unique:  2
#> 
#>      Item      Count   Percent   Cum. Count   Cum. Percent
#> ---  -------  ------  --------  -----------  -------------
#> 1    Male        277     55.4%          277          55.4%
#> 2    Female      223     44.6%          500         100.0%
```

This could also have been done with `dplyr` syntax, since `freq()` supports tidy evaluation:

```r
unclean %>% 
  freq(clean_factor(gender,
                    levels = c("^m" = "Male", "^f" = "Female")))
# or:
unclean %>% 
  pull(gender) %>% 
  clean_factor(c("^m" = "Male", "^f" = "Female")) %>% 
  freq()
```

## Speed

The cleaning functions are tremendously fast, because they rely on R's own internal C++ libraries:

```r
# Create a vector with 500,000 items
n <- 500000
values <- paste0(sample(c("yes", "no"), n, replace = TRUE), 
                 as.integer(runif(n, 0, 10000)))

# data looks like:
values[1:3]
#> [1] "no3697"  "yes1906" "yes6738"

clean_logical(values[1:3])
#> [1] FALSE  TRUE  TRUE

clean_character(values[1:3])
#> [1] "no"  "yes" "yes"

clean_numeric(values[1:3])
#> [1] 3697 1906 6738

# benchmark the cleaning based on 10 runs and show it in seconds:
microbenchmark::microbenchmark(logical = clean_logical(values),
                               character = clean_character(values),
                               numeric = clean_numeric(values),
                               times = 10,
                               unit = "s")
#> Unit: seconds
#> expr            min        lq      mean    median        uq       max neval
#> logical   0.2846163 0.2925479 0.3076008 0.3100244 0.3189712 0.3269428    10
#> character 0.4522698 0.4593437 0.4734631 0.4636837 0.4888959 0.5303473    10
#> numeric   0.6428362 0.6476207 0.6618845 0.6542312 0.6778215 0.6897005    10
```

Cleaning 500,000 values (!) only takes 0.3-0.6 seconds on our system.

## Invalid regular expressions

If invalid regular expressions are used, the cleaning functions will not throw errors, but instead will show a warning and will interpret the expression as a fixed value:

```r
clean_character("0123test 0123[a-b] ")
#> [1] "test ab"

clean_character("0123test 0123[a-b] ", remove = "[a-b]")
#> [1] "0123test 0123[-]"

clean_character("0123test0123", remove = "[a-b")
#> [1] "0123test 0123]"
#> Warning message:
#> invalid regular expression '[a-b', reason 'Missing ']'' - now interpreting as fixed value 
```
