---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date within a software is often required when you want to log activities, apply time stamps or query data based on date. C programmers do it to keep track of real-time events or schedule tasks.

## How to:

C library provides `time.h` header file that houses several functions to get and manipulate date and time. Here's a quick example:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    printf("Current date: %02d-%02d-%04d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);

    return 0;
}
```
This will output:

```C
Current date: xx-xx-xxxx
```
'Replace the `xx-xx-xxxx` with the current date.

## Deep Dive:

Using `time.h` for getting the current date is standard from C89/C90 standard. However, beware of the `tm_year` field as it is years since 1900. Also, `tm_mon` operates with range 0-11, these idiosyncrasies cause confusion.

An alternative for getting the current date is using external libraries like `boost` in C++, but it tends to increase complexity with its heavy features.

Implementation-wise, `time.h` generates the date based on your operating system's Internal Clock. Remember the precision is up to seconds and doesn't cover milliseconds.

## See Also:

- C library function - time(): https://www.tutorialspoint.com/c_standard_library/c_function_time.htm
- struct tm: https://www.tutorialspoint.com/c_standard_library/c_struct_tm.htm
- C date and time programming: https://en.wikibooks.org/wiki/C_Programming/time.h
- Boost Date Time Library: https://www.boost.org/doc/libs/1_72_0/doc/html/date_time.html