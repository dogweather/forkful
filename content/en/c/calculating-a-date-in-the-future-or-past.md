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

## What & Why?
Calculating a date in the future or past refers to performing mathematical operations on a given date to obtain a new date that is either in the future or the past. Programmers often do this to perform date-related calculations in their code, such as generating expiration dates, scheduling tasks, or calculating timelines.

## How to:
To calculate a future or past date in C, we can use the `time.h` library. This library provides functions for working with dates and times in C. Here's an example code to calculate a date 10 days from today:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // obtain the current date
  time_t currentTime;
  time(&currentTime);

  // set the desired offset, here we choose 10 days
  int daysOffset = 10;

  // calculate the future date
  time_t futureTime = currentTime + (daysOffset * 24 * 60 * 60);

  // print the date in a readable format
  printf("The date 10 days from today is: %s", ctime(&futureTime));
}
```

Output:
```
The date 10 days from today is: Mon Aug 09 00:00:00 2021
```
Similarly, we can calculate a date in the past by using a negative offset.

## Deep Dive:
Historically, calculating dates has been a complex task due to differences in calendars and timekeeping systems. However, the ISO 8601 standard, which specifies the internationally accepted date and time format, has made it easier to perform date calculations across different systems. Other alternatives for calculating dates in C include using the `chrono` library from C++ and using external libraries such as `libdate` or `libtspd`.

When calculating dates in C, it's important to take into account leap years and timezones, especially if the calculation is for a specific location or event. This can be achieved by considering the daylight saving time changes and using built-in functions such as `mktime` and `localtime` to handle different timezones accurately.

## See Also:
- [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
- [C time.h library documentation](https://www.tutorialspoint.com/c_standard_library/time_h.htm)