---
title:                "Calculating a date in the future or past"
html_title:           "C recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# The Programming Path: Calculating Dates in C

## What & Why?
Calculating a future or past date is simply determining what the date will be or was after or before a specific period. Coders frequently use this operation in programs that manage tasks, events, or any time-based activities.

## How to:
There are various ways to calculate dates in C, but we'll use `mktime()` and `localtime()` functions from `time.h` in our code example. Note: Adjustments for daylight saving time are made automatically.

```C
#include<stdio.h>
#include<time.h>

int main() {
    time_t raw;
    struct tm * timeinfo;

    time (&raw);                      // get current time
    timeinfo = localtime (&raw);
    printf ("Today's date: %s", asctime(timeinfo));

    timeinfo->tm_mday += 5;           // add 5 days to the date
    mktime (timeinfo);
    printf ("Future date: %s", asctime(timeinfo));

    return 0;
}
```
This code prints the current date and the date 5 days from now. Enjoy playing with it!

## Deep Dive
Historically, date calculations in C were not always straightforward. Early coders had to manually consider aspects like leap years or variable month lengths. Over time, functions in libraries such as `time.h` have simplified this task significantly.

Alternatives to using `mktime()` and `localtime()` include `difftime()` and `strftime()`. You could also use libraries like Boost or date.h if you’re comfortable adding dependencies to your project.

Implementation details worth noting: The `mktime()` function normalizes all the fields of the `tm` structure. This means if you add 40 to the days field, `mktime()` will account for this overflow and update the month and year fields accordingly. Remember, `mktime()` considers Daylight Saving Time. If you find any inconsistencies in your calculation, time zones and DST could be the culprits. 

## See Also
- [C Library - <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Boost Date-Time Library](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [date.h](https://github.com/HowardHinnant/date)