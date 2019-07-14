% clean

# `clean`
**The R package for fast and easy data set cleaning and checking, with very few dependencies.**

## Why this package
As a data scientist, I'm often served with data that is not clean, not tidy and consquently not ready for analysis at all. For tidying data, there's of course the `tidyverse` (https://www.tidyverse.org), which lets you manipulate data in any way you can think of. But for *cleaning*, I think our community was still lacking a neat solution that makes data cleaning fast and easy with functions that kind of 'think on their own' to do that. Cleaning with smart guessing, but with user options to override anything if needed.

## How it works
This package provides two types of functions: **cleaning** and **checking**.

### Cleaning

Use `clean()` to clean data. Under the bonnet (or hood, whatever you prefer) it guesses what kind of data class would best fit your input data. It calls any of these, which can also be used independently:
  
* `clean_logical()` for values `TRUE`/`FALSE`. You only define what should be `TRUE` or `FALSE` and it handles the rest for you. At default, values starting with a Y or J are considered `TRUE` and values starting with an N are considered `FALSE`.
  ```r
  clean_logical(c("Yes", "No", "Invalid", "Unknown"))
  #> [1]  TRUE FALSE    NA    NA
  ```
  If you define the `true` and `false` parameters yourself, they will be interpreted as regular expressions:

  ```r
  clean_logical(x = c("Positive", "Negative", "Unknown", "Some value"),
                true = "pos",
                false = "neg")
  #> [1]  TRUE FALSE    NA    NA
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
  #> Cleaning dates using format 'ddmmyyyy'
  #> [1] "2012-06-12"
  
  clean_Date(38071)
  #> Cleaning dates using Excel format
  #> [1] "2004-03-25"
  ```
  
  The function to transform `d-mmm-yyyy` to `%e-%b-%Y` is available as `format_datetime()` to users. This makes it possible to use it in other date functions too:
  
  ```r
  as.Date("12-13-14", format = format_datetime("mm-yy-dd"))
  #> [1] "2013-12-14"
  ```
  
* `clean_numeric()` to keep numbers from cluttered input text:
  
  ```r
  clean_numeric("qwerty123456")
  #> [1] 123456
  
  clean_numeric("Positive (0.143")
  #> [1] 0.143
  ```
  
* `clean_character()` to keep characters from cluttered input text:
  
  ```r
  clean_character("qwerty123456")
  #> [1] "qwerty"
  
  clean_character("Positive (0.143")
  #> [1] "Positive"
  ```
  
### Checking

Use `freq()` to create comprehensive frequency tables to check your data. The function supports a lot of different classes (types of data) and is even extendible by other packages.

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

This could also have been done with `dplyr` syntax, since `freq()` support tidy evaluation:
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
