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

## Why
Calculating dates in the future or past may seem like a trivial task, but it can actually be quite useful in various applications. Imagine needing to schedule an event or program a task that should occur on a specific date - having the ability to calculate future or past dates programmatically can save time and effort.

## How To
To calculate a date in the future or past, we need to use the `time.h` library in C. This library includes functions for date and time manipulation. We'll be using the `mktime` function to convert a given date into a time in seconds, and the `localtime` function to convert back to a structured time (including date). Let's take a look at a simple example:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Create a structured time for 14th September 2021, 10:30 AM
  struct tm date = {0};
  date.tm_mday = 14;  // Day of the month (1-31)
  date.tm_mon = 8;    // Month (0-11)
  date.tm_year = 121; // Year (current year minus 1900)
  date.tm_hour = 10;  // Hour (0-23)
  date.tm_min = 30;   // Minute (0-59)

  // Convert the structured time to seconds
  time_t date_seconds = mktime(&date);

  // Calculate a future date by adding 2 weeks (in seconds)
  // Note: we could also add or subtract any other unit of time (e.g. days, hours)
  time_t future_date = date_seconds + (2 * 7 * 24 * 60 * 60);

  // Convert back to a structured time
  // Note: we can also use gmtime instead of localtime to get universal time
  struct tm *future_time = localtime(&future_date);

  // Output the future date in a human-readable format
  printf("The future date is: %02d/%02d/%d at %02d:%02d\n",
          future_time->tm_mday, future_time->tm_mon + 1,
          future_time->tm_year + 1900, future_time->tm_hour, future_time->tm_min);

  return 0;
}
```
You can modify the values for `date` and the unit of time used in `future_date` to experiment with different results. For example, you can calculate a date in the past by subtracting instead of adding.

The output of the example above would be:
```
The future date is: 28/09/2021 at 10:30
```

## Deep Dive
The `time.h` library in C also provides other useful functions for date and time manipulation, such as `difftime` for calculating the difference between two times in seconds, `strftime` for formatting time strings, and `gmtime` for getting universal time. Additionally, there are other libraries and APIs available for date and time calculations, such as `datetime.h` from the C++ standard library and the date and time features of the POSIX standard.

See Also
- [C Date and Time Functions](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm)
- [C++ Date and Time Functions](https://www.geeksforgeeks.org/c-datetime-function-examples/)
- [POSIX Time Functions](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time-Functions.html)