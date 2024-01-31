---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:35:03.035290-07:00
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means to extract and convert the date expressed as text into a structured format that a program can understand and work with. Programmers do it because dates in text form aren't handy for calculations, comparisons, or storing in a standardized format.

## How to:

Here's a tiny guide on parsing a date string in C using `strptime()` from `time.h`. It reads the date in the format `"YYYY-MM-DD"` and turns it into a `struct tm`.

```C
#include <stdio.h>
#include <time.h>

int main() {
    const char *date_str = "2023-03-14";
    struct tm tm;
    
    // Clear struct to avoid garbage values
    memset(&tm, 0, sizeof(struct tm));
    
    // Parse the date string
    if (strptime(date_str, "%Y-%m-%d", &tm) == NULL) {
        printf("Date parsing failed.\n");
        return 1;
    }

    // Print the parsed date
    printf("Year: %d, Month: %d, Day: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);

    return 0;
}
```

Sample output:
```
Year: 2023, Month: 3, Day: 14
```

## Deep Dive

Once upon a time, dates were a mess to handle in C, with programmers parsing strings manually by fiddling with `strtok()`, `sscanf()` or even raw loops and character checks. But then `strptime()` rolled in as part of POSIX, allowing us to convert strings representing time to `struct tm` with predefined formats.

Alternatives like `getdate()` exist but aren't used as widely. And there's the manual way - directly manipulating strings but let's not go back to the dark ages, okay?

Implementation wise, `strptime()` does need you to clear out your `struct tm` because it won't do it for you. If you skip that zeroing out using `memset()`, you might get random garbage in the unused fields, leading to unexpected results.

Remember, `strptime()` is part of POSIX, so if you're on a non-POSIX system like Windows, you'll need to look for a different solution or a compatibility layer, like `win32` implementations or third-party libraries.

## See Also

- [C++ `<chrono>` Library](https://en.cppreference.com/w/cpp/header/chrono)
For those also dabbling in C++ and seeking a more modern take on date and time manipulation.

Though the focus here is C, a deeper understanding of POSIX time functions is always a plus.

- [strftime and strptime Behavior](https://man7.org/linux/man-pages/man3/strptime.3.html)
The man page for `strptime()` and `strftime()` for understanding how to format time in C.

When playing with times and dates, watch out for timezones and daylight saving changes — these can throw a wrench into the works when not handled properly. Happy coding!
