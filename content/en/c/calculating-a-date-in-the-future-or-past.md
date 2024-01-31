---
title:                "Calculating a date in the future or past"
date:                  2024-01-20T17:28:35.632876-07:00
model:                 gpt-4-1106-preview
html_title:           "C# recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a future or past date involves figuring out the exact day that's a specific interval away from a known date. Programmers do this for scheduling events, expiring tokens, reminders, etc.

## How to:

Here's straight-up C code to calculate a date in the future. We're using `time.h` functions.

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm new_date;
    double daysToAdd = 10; // 10 days into the future

    // Get current time and convert to tm struct
    time(&now);
    new_date = *localtime(&now);

    // Adding the days to the current date
    new_date.tm_mday += daysToAdd;
    mktime(&new_date);

    // Output the new date:
    printf("The date in 10 days will be: %02d-%02d-%04d\n",
           new_date.tm_mday,
           new_date.tm_mon + 1, // tm_mon is 0-11
           new_date.tm_year + 1900); // tm_year is years since 1900

    return 0;
}
```

Sample output: `The date in 10 days will be: 12-04-2023`

## Deep Dive

Back in the day, calculating future or past dates was a hassle - no built-in functions, just pure algorithmic fun. Now, C's `time.h` gives you `time_t`, `struct tm`, and functions like `mktime()` to make life easier.

Alternatives? You bet. For complex date-time manipulation, some devs go for libraries like `date.h` for C++ or the 'chrono' module. 

The details? `mktime()` normalizes `struct tm`. Means if you add 40 to days, it rolls over months, even years. Good to know, lest you invent your own time machine going in circles.

## See Also

- C Standard Library - `time.h`: https://en.cppreference.com/w/c/chrono
- Alternative date and time libraries, like Howard Hinnant's `date.h` library for C++: https://github.com/HowardHinnant/date
- `mktime()` function explanations: https://www.cplusplus.com/reference/ctime/mktime/
