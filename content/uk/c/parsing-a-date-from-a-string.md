---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:34:45.059656-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Parsing a date from a string means pulling out date information from text. Programmers do this to make dates usable for operations like comparisons, sorting, or storage in a standardized format.

## How to: (Як це зробити:)
```C
#include <stdio.h>
#include <time.h>

int main() {
    const char *date_str = "2023-03-15";
    struct tm tm;
    if (strptime(date_str, "%Y-%m-%d", &tm)) {
        printf("Year: %d, Month: %d, Day: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
    } else {
        printf("Failed to parse the date.\n");
    }
    return 0;
}
```
Sample Output:
```
Year: 2023, Month: 3, Day: 15
```

## Deep Dive (Поглиблений Аналіз)
Parsing dates from strings is common in C, especially before JSON became prevalent. In the early days, programmers mostly handled dates manually. `strptime()` is now our go-to for parsing, introduced in POSIX. It's not part of standard C, so check for compatibility. Alternatives? `sscanf()` - more manual, less safe. `strptime()` lets us specify the expected format and handles various locales. Implementation-wise, it fills a `struct tm` with info, which we can use as needed.

## See Also (Дивіться також)
- C Standard Library documentation: https://en.cppreference.com/w/c
- POSIX `strptime` function details: http://man7.org/linux/man-pages/man3/strptime.3.html
- Date and time tutorial in C: https://www.tutorialspoint.com/c_standard_library/c_function_strptime.htm