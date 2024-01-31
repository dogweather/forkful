---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:35:03.894134-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Co i Dlaczego?
Parsing dates from strings is about converting text to a date/time format that a program can understand. Programmers do it to process user input, to interact with databases, and to handle date logic in applications.

## How to / Jak to zrobić:
Here's a simple example of how to do it in C using the `strptime` function:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char *str_date = "2023-03-14";
    
    if (strptime(str_date, "%Y-%m-%d", &tm) == NULL) {
        printf("Date format error\n");
    } else {
        printf("Successfully parsed: Year: %d, Month: %d, Day: %d\n", 
               tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
    }
    
    return 0;
}
```

Output:
```
Successfully parsed: Year: 2023, Month: 3, Day: 14
```

Note: `tm.tm_year` is the number of years since 1900, and `tm.tm_mon` is zero-indexed (0 = January).

## Deep Dive / W Głąb Tematu:
Parsing a date from a string isn't new. In C, `strptime()` has been the go-to since POSIX standards came into play. There are alternatives, like `sscanf` or third-party libraries, but they have trade-offs in complexity and safety.

The `strptime` function's job is to translate a string form based on provided format directives (like `%Y` for year). It populates a `tm` struct with this data. However, remember it doesn't set `tm.tm_wday` or `tm.tm_yday` unless you explicitly use day of the week or day of the year in your format string.

Also, mind the locale. `strptime` behavior may differ with locale settings regarding day/month names.

## See Also / Zobacz Również:
- C Standard Library Documentation on `strptime`: https://en.cppreference.com/w/c/chrono/strptime
- GNU C Library Reference Manual: https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html
- POSIX Standard for Time Functions: https://pubs.opengroup.org/onlinepubs/9699919799/functions/strptime.html
