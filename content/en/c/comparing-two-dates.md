---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

# What & Why?

Comparing two dates entails evaluating whether a date is earlier, later, or equal to another, enabling us to create time-sensitive functionalities. It's essential in tasks like sorting events by date, calculating elapsed time, or scheduling.

# How To:

Below, a simple method to compare two dates in C using the `time.h` library. 

```C
#include<stdio.h>
#include<time.h>

int main()
{
    struct tm date1 = {0, 0, 0, 15, 10, 2021 - 1900}; 
    struct tm date2 = {0, 0, 0, 25, 10, 2021 - 1900};
    
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);
    
    if (time1 < time2)
        printf("Date1 is earlier than Date2\n");
    else if (time1 > time2)
        printf("Date2 is earlier than Date1\n");
    else
        printf("Dates are equal\n");
        
    return 0;
}
```

Output:

```C
Date1 is earlier than Date2
```

This program first creates two `struct tm` instances, representing two dates. We convert these into `time_t` values using `mktime()`, which we can compare using standard relational operators.

# Deep Dive 

The C library's `time.h` has been handling date and time since its inception. Before C99, it only had `time_t` for date/time representation. C99 added `struct timespec` for higher precision. 

Several other alternatives to `time.h` exist. For instance, `chrono` library in C++ supplies in-depth time functions. Third-party libraries like Boost or ICU offer richer, though more complex, solutions.

The magic value '1900' in the struct initialization is due to `tm_year`'s definition: it's the number of years since 1900. The instances of `struct tm` are then converted to `time_t` using `mktime()` which accounts for leap years, variable month lengths, and time zones.

# See Also 

Check these for more:
- Manual pages for time: https://man7.org/linux/man-pages/man7/time.7.html
- StackOverflow post on date comparison: https://stackoverflow.com/questions/3556323/comparing-two-dates-in-c 
- POSIX timers overview: http://man7.org/linux/man-pages/man7/time.7.html