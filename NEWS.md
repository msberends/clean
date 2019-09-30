# clean 1.1.0.9000

* Added support for percentages as a new class: `as.percentage()` and `clean_percentage()`. They also come with 'S3 methods' for `print`, `format`, `sum`, `min` and `max`.
* More robust coercing of dates
* Support for negative values for `clean_numeric()`, `clean_percentage()` and `clean_currency()`
* Fix for `clean_character()` on R v3.5 and lower
* Fix for digits in frequency tables for numeric values

# clean 1.1.0

* Added support for currency as a new class: `as.currency()` and `clean_currency()`. They also come with 'S3 methods' for `print`, `format`, `sum`, `min` and `max`.
* Added `clean_POSIXct()` to clean date/time objects
* `top_freq()` now correctly selects bottoms items using negative a number for *n*
* `freq.default()` is now exported for use in other packages
* All numeric calculation in the header of frequency tables now use the same algorithm as used by Minitab and SPSS (see 'Type 6' on `stats::quantile()`)
* More robust results for `clean_character()`, it also keeps in-between spaces now
* `clean_numeric()` now supports currency
* Fix for `freq()` where the precentage of NAs in the header was not calculated right

# clean 1.0.0

* First release
