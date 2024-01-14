---
title:                "C recipe: Comparing two dates"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in many programming projects, especially when dealing with events or scheduling. It allows you to determine the chronological order of events and make decisions based on date comparisons. In this blog post, we will explore how to compare two dates in the C programming language.

## How To

First, we need to understand how dates are represented in C. Dates are often stored as integers in the format of `YYYYMMDD`, where `YYYY` is the year, `MM` is the month, and `DD` is the day. For example, January 1st, 2020 would be represented as `20200101`.

To compare two dates, we first need to have two date variables. Let's call them `date1` and `date2`. We can compare them using simple if-else statements and logical operators.

```C
if (date1 > date2) {
  printf("Date 1 is later than Date 2");
}
else if (date1 < date2) {
  printf("Date 1 is earlier than Date 2");
}
else {
  printf("Date 1 and Date 2 are the same");
}
```

Depending on the comparison, either "Date 1 is later than Date 2", "Date 1 is earlier than Date 2", or "Date 1 and Date 2 are the same" will be printed to the console.

We can also compare dates using the `difftime()` function from the `time.h` library. This function calculates the difference in seconds between two dates.

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t date1 = time(NULL); // current date in seconds
  time_t date2 = 1585718400; // April 1st, 2020 in seconds

  double diff = difftime(date1, date2); // difference in seconds
  printf("Difference between Date 1 and Date 2 is %f seconds", diff);
  
  return 0;
}
```

The output will be "Difference between Date 1 and Date 2 is 1585718400 seconds", which is equivalent to approximately 49 years.

## Deep Dive

In some cases, we may need to compare specific components of dates, such as the year, month, or day. To do this, we can use the `tm` structure from the `time.h` library.

```C
#include <stdio.h>
#include <time.h>

int main() {
  // current date and time
  time_t now = time(NULL);
  
  struct tm *local = localtime(&now); // Local time
  struct tm *utc = gmtime(&now); // UTC time
  
  // Print current year, month, and day in each time zone
  printf("Local: %d-%d-%d\n", local->tm_year + 1900, local->tm_mon + 1, local->tm_mday);
  printf("UTC: %d-%d-%d\n", utc->tm_year + 1900, utc->tm_mon + 1, utc->tm_mday);
  
  return 0;
}
```

The output will be something like:

```
Local: 2020-04-16
UTC: 2020-04-16
```

We can compare the individual components of the dates using logical operators, just like we did in the first example.

## See Also

To learn more about dates and time in C, check out these resources:

- [C Programming Tutorial: Date and Time Functions](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [The `time.h` header](https://www.codingunit.com/c-tutorial-the-time-h-header)
- [Date and Time in C](https://www.programiz.com/c-programming/c-date-time)