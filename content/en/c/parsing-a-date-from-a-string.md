---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of taking a date in the form of a string and converting it into a format that a computer can understand. This is important for programmers as it allows them to manipulate and perform operations on dates, which are frequently used in many applications.

## How to:

To parse a date from a string in C, we can use the `strptime()` function from the standard library. This function takes in a string, a format string, and a pointer to a `tm` struct, and returns a pointer to that struct with the parsed date information.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Input string and format
    char str[] = "February 4, 2020";
    char format[] = "%B %d, %Y";

    // Create tm struct
    struct tm parsed_date;

    // Parse string into tm struct
    strptime(str, format, &parsed_date);

    // Print parsed date information
    printf("Month: %d\n", parsed_date.tm_mon + 1); // tm_mon is indexed from 0
    printf("Day: %d\n", parsed_date.tm_mday);
    printf("Year: %d\n", parsed_date.tm_year + 1900); // tm_year is in years since 1900

    return 0;
}
```

**Output:**
```
Month: 2
Day: 4
Year: 2020
```

## Deep Dive:

Parsing dates from strings can be tricky due to the many different date formats that exist. In the past, programmers had to manually write code to parse specific date formats, but the `strptime()` function makes this process much easier. Additionally, there are alternative libraries and functions available for date parsing, such as `strftime()` and `strptime_l()`, which provide more flexibility and options.

When parsing a date from a string, it is important to match the format string to the input string exactly, otherwise the `strptime()` function will return an error. The `tm` struct used in the example above stores the parsed date information in its members, such as `tm_mon` for the month and `tm_mday` for the day.

## See Also:

- [`strptime()` documentation](https://www.man7.org/linux/man-pages/man3/strptime.3.html)
- [`strptime_l()` documentation](https://www.man7.org/linux/man-pages/man3/strptime_l.3.html)
- [`strftime()` documentation](https://www.man7.org/linux/man-pages/man3/strftime.3.html)