---
title:    "C recipe: Converting a date into a string"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a trivial task, but it can actually be quite useful in certain situations. For example, if you are working on a project that requires date inputs from users, it may be more convenient to have those inputs in string format for easier manipulation and validation. Additionally, converting dates into strings can also be helpful for displaying date information in a specific format.

## How To

To convert a date into a string in C programming, we can use the `strftime()` function from the `time.h` library. This function allows us to format a given date according to a specified format and returns a string representation of the date.

Let's take a look at an example:

```C
#include <stdio.h>
#include <time.h>

int main() {
   // obtain current time
   time_t now = time(NULL);

   // declare buffer to store string
   char buffer[100];

   // format date to string
   strftime(buffer, sizeof(buffer), "%B %d, %Y", localtime(&now));

   // print string
   printf("Current date: %s\n", buffer);

   return 0;
}
```

**Output:**

Current date: June 01, 2021

In this example, we used the `%B` format specifier to get the full month name, `%d` for the day of the month, and `%Y` for the full year. You can refer to the [strftime documentation](https://www.cplusplus.com/reference/ctime/strftime/) for a complete list of format specifiers.

## Deep Dive

While `strftime()` is a convenient function for converting dates into strings, it is important to note that the format of the string output may vary depending on the system's locale settings. This means that the same format specifier may produce different results on different systems.

To ensure consistency in the output, we can set the locale using the `setlocale()` function before calling `strftime()`:

```C
setlocale(LC_ALL, "en_US"); // set locale to U.S.
```

Additionally, `strftime()` only works for dates within the range of January 1, 1970 and December 31, 2037 on most systems due to the `time_t` data type used in the function. For dates outside of this range, we can use the `mktime()` function to convert a `tm` struct into a `time_t` value that can be passed into `strftime()`.

## See Also

- [strftime documentation](https://www.cplusplus.com/reference/ctime/strftime/)
- [mktime documentation](https://www.cplusplus.com/reference/ctime/mktime/)
- [setlocale documentation](https://www.cplusplus.com/reference/clocale/setlocale/)