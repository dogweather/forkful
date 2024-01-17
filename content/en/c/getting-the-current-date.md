---
title:                "Getting the current date"
html_title:           "C recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in C is a way for programmers to obtain the current date and time on a computer. This information is important for various reasons, such as tracking when a program was executed or organizing data in a chronological manner.

## How to:

To get the current date in C, we can use the standard library function, `time()`. This function returns the number of seconds that have passed since January 1, 1970. Here's an example of using `time()` to print the current date and time:

```
#include <stdio.h>
#include <time.h>

int main() {
  time_t currentTime;
  struct tm *localTime;

  // Getting current time
  currentTime = time(NULL);

  // Converting to local time
  localTime = localtime(&currentTime);

  // Printing the current date and time
  printf("Current date and time: %s", asctime(localTime));

  return 0;
}
```

The output of this code snippet will be something like this:

```
Current date and time: Sun Oct 17 21:30:02 2021
```

## Deep Dive:

The use of `time()` function for getting the current date can be traced back to the Unix operating system, where it was first implemented. It has since become a standard function in various programming languages and is widely used for time tracking and other related tasks.

Alternative ways to get the date and time include using third-party libraries or APIs that provide more features and customization options. However, the `time()` function is a reliable and straightforward solution for most basic use cases.

Behind the scenes, `time()` makes use of the computer's internal clock or Real Time Clock (RTC). The clock is a hardware component that keeps track of the current time and date. The `time()` function retrieves this information and converts it into seconds since the Unix epoch (January 1, 1970).

## See Also:

- [C time functions](https://www.tutorialspoint.com/c_standard_library/time_h.htm): A comprehensive list of time-related functions in the standard C library.
- [Using a third-party library for getting current date and time](https://www.gnu.org/software/libc/manual/html_node/Calendar-Time.html#Calendar-Time): An alternative approach using the GNU C Library.