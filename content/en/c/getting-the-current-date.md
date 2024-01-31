---
title:                "Getting the current date"
date:                  2024-01-20T15:12:57.271115-07:00
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date means finding out today's date as per the system's internal clock. Programmers do this to stamp logs, validate events, and time-stamp data.

## How to:

You'll want to include `time.h` to deal with time in C.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    
    printf("Current Date: %02d-%02d-%d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    
    return 0;
}
```

Sample output:
```
Current Date: 15-04-2023
```

## Deep Dive

Historically, dealing with time in C goes way back to the early days of UNIX, thanks to C's strong system-level capabilities. For current dates, we rely on the `time.h` library, which has been around since C was standardized by ANSI. 

The `time_t` type stores the current time since Epoch (00:00:00 UTC on 1 January 1970) in seconds. The `localtime` function translates this time into a `struct tm` that holds calendar date and time broken down into its components.

Alternatives? There are other ways to manipulate and represent time in C. For instance, `gmtime` converts `time_t` to coordinated universal time (UTC) instead of local time, which `localtime` does. Using `strftime`, you can customize your date and time format extensively.

As for details, `time_t` is typically an integer or a floating-point type. Implementation can vary across systems but the standard doesn't mandate the precise type, just that it's capable of representing times.

When using time-related functions, remember to consider daylight saving time and locale-specific data if your application is sensitive to those.

## See Also

- The GNU C Library Reference Manual on Time: https://www.gnu.org/software/libc/manual/html_node/Time.html 
- C Standard Library - time.h: https://en.cppreference.com/w/c/chrono 
- Learn more about time formats with strftime: https://en.cppreference.com/w/c/chrono/strftime
