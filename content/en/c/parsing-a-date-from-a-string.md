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
Parsing a date from string in C is the process of converting a string that represents a date into a data structure that represents the date. This is done to facilitate calculations, sorting, and many other operations that are difficult to do on dates represented as strings.

## How to:
We can parse a date from a string by using the `strptime` function in `time.h`. Below is a basic example of how to use it:

```C
#include <time.h>
#include <stdio.h>

int main() {
    struct tm tm;
    char buf[255];

    strptime("2022-06-24 14:45", "%Y-%m-%d %H:%M", &tm);
    strftime(buf, sizeof(buf), "%d %B %Y %H:%M", &tm);

    printf("Parsed date: %s\n", buf);

    return 0;
}
```

When you run this program, it will output:

```bash
Parsed date: 24 June 2022 14:45
```

This indicates that the date string "2022-06-24 14:45" has been successfully parsed into a `struct tm` object.

## Deep Dive
Historically, the parsing of dates has been a tedious process due to the varied date formats across different locales. Things got easier with the introduction of the `time.h` library in C89, which included the `strptime` function.

An alternative approach, albeit less portable, is to manually parse the string using other string manipulation functions like `strtok`. You then convert these individual string components into integers for the day, month, and year.

Take note that the `strptime` function takes a format string, similar to `printf` and `scanf`, to interpret the input string. The resulting `struct tm` struct can be further used to output the formatted date or for internal computations.

## See Also
- C Library - <ctime>: https://www.cplusplus.com/reference/ctime/
- `strftime` function: https://www.cplusplus.com/reference/ctime/strftime/
- `strptime` function: https://www.cplusplus.com/reference/ctime/strptime/