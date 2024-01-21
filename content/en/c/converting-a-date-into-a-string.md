---
title:                "Converting a date into a string"
date:                  2024-01-20T17:36:16.234028-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a date into a string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
We turn dates into strings to make them human-readable or to format them for storage and display. It's about taking raw date data and presenting it in a way that makes sense to us.

## How to:
C makes this job pretty straightforward with the `strftime` function. Here's a quick example:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t rawtime;
    struct tm * timeinfo;
    char buffer[80];

    time(&rawtime);
    timeinfo = localtime(&rawtime);

    strftime(buffer, sizeof(buffer), "%d-%m-%Y %I:%M:%S", timeinfo);
    printf("Formatted date & time: %s\n", buffer);

    return 0;
}
```

Sample output could be: `Formatted date & time: 22-03-2023 09:45:12`

## Deep Dive:
Historically, C's time-handling has its quirks: earlier standards lacked a standardized way to handle time zones, for instance. Now, we've got `strftime` as part of the Standard C Library from C89 onwards, giving us a consistent way to turn `struct tm` time structures into strings, with format control.

As for alternatives, one could manually extract values from `struct tm` and concatenate them, but that’s reinventing the wheel. There's also the POSIX `strptime` function, which goes in reverse, string to `struct tm`.

When using `strftime`, remember: buffer size matters; too small and your string gets cut off. Also, the format specifiers in `strftime` allow you to customize the date and time in various human-friendly ways, like changing locales or the date-time representation.

## See Also:
- C Standard Library documentation: https://en.cppreference.com/w/c/chrono/strftime
- GNU C Library Manual on Time: https://www.gnu.org/software/libc/manual/html_node/Time.html
- strftime format specifiers: https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html#Low_002dLevel-Time-String-Parsing