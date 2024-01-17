---
title:                "Comparing two dates"
html_title:           "C recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates in programming involves checking if one date is before, after, or equal to another date. Programmers do this to perform operations such as sorting and filtering data, calculating time differences, and scheduling tasks.

## How to:
Here's a simple example of how to compare two dates in C:

```
#include <stdio.h>
#include <time.h>

int main() {
    // Creating two structs to represent dates
    struct tm date1 = { .tm_mday = 25, .tm_mon = 11, .tm_year = 2020};
    struct tm date2 = { .tm_mday = 1, .tm_mon = 2, .tm_year = 2021 };

    // Converting structs to time_t type for comparison
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Using difftime() function to compare time difference in seconds
    double diff = difftime(time2, time1);

    // Outputting result
    if (diff > 0) {
        printf("Date 2 is after Date 1.");
    } else if (diff < 0) {
        printf("Date 2 is before Date 1.");
    } else {
        printf("Date 1 and Date 2 are equal.");
    }

    return 0;
}
```
**Output:**
```
Date 2 is after Date 1.
```

## Deep Dive:

### Historical Context:
In older versions of C, dates were commonly represented as a quantity of seconds since a specific epoch time. However, this could cause issues with different interpretations of leap years and daylight saving time. Therefore, the use of structured data types, such as tm, was introduced in the C standard library.

### Alternatives:
Apart from the difftime() function used in the example, there are other ways to compare two dates in C. One alternative is to use the timecmp() function from the POSIX standard library. Another option is to use a library specifically designed for handling dates and time, such as Boost.Date_Time or GLib GDateTime.

### Implementation details:
In C, dates can be represented using the struct tm data type, which contains information such as year, month, day, and time. The mktime() function is used to convert this data type into a time_t type, which represents a specific point in time. The difftime() function then calculates the difference in seconds between two time_t values, allowing for easy comparison.

## See Also:
- [C Standard Library - Date and Time Functions](https://www.cplusplus.com/reference/ctime/)
- [POSIX Time Functions](https://pubs.opengroup.org/onlinepubs/9699919799/functions/index.html)
- [Boost.Date_Time library](https://www.boost.org/doc/libs/1_74_0/doc/html/date_time.html)
- [GLib GDateTime reference](https://developer.gnome.org/glib/stable/glib-Dates-and-Times.html)