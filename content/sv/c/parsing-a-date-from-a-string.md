---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:34:59.273630-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att tolka ett datum från en sträng innebär att konvertera text till ett datumformat som datorn kan förstå och hantera. Programmerare gör detta för att möjliggöra bearbetning och jämförelse av datum som användare matar in eller som kommer från olika källor som textfiler.

## How to: (Hur gör man?)
```c
#include <stdio.h>
#include <time.h>

int main() {
    const char *dateString = "2023-03-21";
    struct tm tm;
    
    if (strptime(dateString, "%Y-%m-%d", &tm)) {
        char formattedDate[20];
        strftime(formattedDate, sizeof(formattedDate), "%d %b %Y", &tm);
        printf("Parsed Date: %s\n", formattedDate);
    } else {
        printf("Could not parse the date.\n");
    }
    
    return 0;
}
```
Sample output:
```
Parsed Date: 21 Mar 2023
```

## Deep Dive (Djupdykning)
Parsing dates from strings has always been crucial for applications that handle events, schedules, or any date-related data. Historically, C programmers had to rely on custom parsing functions because the standard library's support was limited. Nowadays, `strptime()` and `strftime()` from `time.h` are widely used — `strptime()` reads a string and fills a `tm` structure with date and time information, while `strftime()` formats it back into a readable string.

Alternatives exist for different contexts. For example, in Windows, you might use `sscanf()` or Windows-specific functions. Moreover, high-level languages tend to have better support for date and time parsing, so using a C library wrapper around a C++ library like Boost.Date_Time is also an option.

Implementation-wise, it’s important to note that `strptime()` is not part of the C standard library, but it’s part of POSIX. Therefore, it might not be available on all platforms.

## See Also (Se även)
- C-standards documentation (https://en.cppreference.com/w/c/chrono)
- GNU C Library Manual for Time Parsing (https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html)
- Time.h in POSIX specification (https://pubs.opengroup.org/onlinepubs/009695399/basedefs/time.h.html)
