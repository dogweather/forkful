---
title:                "Comparing two dates"
date:                  2024-01-20T17:32:19.461654-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparing two dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is all about figuring out their chronology—are they the same, is one earlier, or is one later? Programmers do this for stuff like sorting events, validating time periods, and handling reservations. It's everyday time-keeping in code.

## How to:

In C, we often use the `time.h` library to deal with dates. Here's a quick example:

```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // Convert to time_t for easy comparison
    time_t t1 = mktime(&date1);
    time_t t2 = mktime(&date2);

    // Compare
    if (t1 < t2) return -1; // date1 is earlier
    if (t1 > t2) return 1;  // date1 is later
    return 0;               // dates are the same
}

int main() {
    // Two dates to compare
    struct tm date1 = { .tm_year = 120, .tm_mon = 5, .tm_mday = 14 }; // 2020-06-14
    struct tm date2 = { .tm_year = 122, .tm_mon = 11, .tm_mday = 3 };  // 2022-12-03

    int result = compare_dates(date1, date2);

    if (result < 0) {
        printf("Date1 is earlier than Date2.\n");
    } else if (result > 0) {
        printf("Date1 is later than Date2.\n");
    } else {
        printf("Date1 is the same as Date2.\n");
    }

    return 0;
}
```

Sample output:
```
Date1 is earlier than Date2.
```

## Deep Dive

Before `time.h` blessed C with standardized time functions, you'd roll your own date comparisons—risky business with leap years and all. Now, `mktime()` and `time_t` are the go-to. They handle the quirks of calendars so you don't have to.

`mktime()` takes your `struct tm` date, with all its human-friendly fields, and squashes it into a `time_t` value. This value represents seconds since the epoch (00:00, Jan 1, 1970, UTC). Once your dates are in `time_t`, it's just number comparison.

There are fancier alternatives, like `difftime()` for finding the time difference or using third-party libraries. They can offer more features but for a straightforward "Which date is earlier?" question, the standard library usually has you covered.

Implementation depends on system time settings—timezones and Daylight Saving Time can trip you up. `mktime()` interprets the `struct tm` as local time, so be mindful when comparing dates from different time zones.

## See Also

- C `time.h` reference: https://en.cppreference.com/w/c/chrono
- `time(7)` - overview of time and date in Unix systems: http://man7.org/linux/man-pages/man7/time.7.html
- GNU C Library (glibc) manual on Time: https://www.gnu.org/software/libc/manual/html_node/Time.html
