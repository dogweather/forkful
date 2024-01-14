---
title:                "C recipe: Getting the current date"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered what day it is or what the current date is? Knowing the current date is important for various reasons, such as for organizing tasks, scheduling events, or simply keeping track of time. In this blog post, we will explore how to get the current date using C programming.

## How To

Getting the current date in C programming involves using the `time.h` library, which provides functions for manipulating time and dates. The most commonly used functions for getting the current date are `time()` and `localtime()`.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // time() returns the time in seconds since 00:00:00 UTC on January 1, 1970
    time_t t = time(NULL);
    
    // localtime() converts the time to a more readable format
    struct tm *now = localtime(&t);
    
    // Print out the current date in a specified format
    printf("Today is %02d/%02d/%d\n", now->tm_mon + 1, now->tm_mday, now->tm_year + 1900);
    
    return 0;
}
```

Running this code will output the current date in the format `MM/DD/YYYY`, for example: `Today is 07/12/2021`.

## Deep Dive

The `time()` function is used to get the current time as a `time_t` object, which represents the number of seconds elapsed since the Unix epoch. This value is then passed as an argument to `localtime()`, which converts it into a structure containing various fields such as year, month, day, and time. The structure is then accessed using the `->` operator to print out the desired date format.

It's important to note that the `localtime()` function uses the local timezone, whereas `gmtime()` uses the Coordinated Universal Time (UTC). Additionally, the `strftime()` function can be used to format the date in various ways, such as displaying the day of the week or the full month name.

## See Also

- [C time and date](https://www.programiz.com/c-programming/library-function/time)
- [How to display current date and time in C](https://www.guru99.com/c-programs-getting-system-date.html)

Getting the current date in C programming is a useful skill to have and can be applied in various scenarios. With the help of the `time.h` library and the functions discussed, you can easily get the current date in your desired format. So go ahead and give it a try in your next C program!