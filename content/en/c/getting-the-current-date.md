---
title:                "C recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever wondered how your computer knows what the current date is? Whether you're curious or looking to incorporate date functions into your C programs, getting the current date can be a useful task to learn. In this blog post, we'll explore the reasons for wanting to get the current date and how to do so in the C programming language.

## How To
To get the current date in C, we'll need to use the `time.h` header file, which contains functions for working with dates and times. Specifically, we'll use the `time()` function, which returns the number of seconds from January 1st, 1970 to the current date and time. We can then use this value to calculate the current date in various formats.

```C
#include <stdio.h>
#include <time.h>

int main() {
  // get current date and time
  time_t now = time(NULL);
  
  // calculate current date and time in different formats
  struct tm *local = localtime(&now);
  
  // long date format (e.g. Friday, October 15, 2021)
  char long_date[20];
  strftime(long_date, 20, "%A, %B %d, %Y", local);
  printf("Long date: %s\n", long_date);
  
  // short date format (e.g. 10/15/2021)
  char short_date[11];
  strftime(short_date, 11, "%m/%d/%Y", local);
  printf("Short date: %s\n", short_date);
  
  // custom date format (e.g. 10-15-21)
  char custom_date[9];
  strftime(custom_date, 9, "%m-%d-%y", local);
  printf("Custom date: %s\n", custom_date);
  
  return 0;
}
```

The output for this program would be:
```
Long date: Friday, October 15, 2021
Short date: 10/15/2021
Custom date: 10-15-21
```

## Deep Dive
As we saw in the code example, the `time()` function returns the number of seconds from a specific date and time (in this case, January 1st, 1970) to the current date and time. This value is known as a "Unix timestamp" and is commonly used in computer systems to indicate dates and times. The `localtime()` function then converts this timestamp into a more readable structure called `struct tm`.

The `strftime()` function, which stands for "string format time", allows us to format the current date and time in various ways using the values stored in `struct tm`. We can specify the date format we want by using symbols such as `%Y` for the four-digit year, `%m` for the month (with leading zero), `%d` for the day (with leading zero), and so on. You can find a full list of these symbols and their meanings in the C documentation for `strftime()`.

## See Also
- [C time and date functions](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C strftime() function](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Unix timestamp](https://www.unixtimestamp.com/)