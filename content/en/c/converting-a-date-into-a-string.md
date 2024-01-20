---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Date to String Conversion in C

## What & Why?

Converting a date into a string in C language refers to representing a date-time as a readable string (text). It's useful for printing dates in logs, reports, and user interfaces.

## How To: 

In C, the function `strftime` is commonly used for this conversion. It formats time as a string.

```C
#include <stdio.h>
#include <time.h>

int main() {
    char buf[80];
    time_t now = time(0);
    struct tm tm = *localtime(&now);
   
    strftime(buf, sizeof buf, "%A %d %B %Y %H:%M:%S", &tm);
    printf("%s\n", buf);
   
    return 0;
}
```

After running the code, the output will resemble the following:

```
Sunday 21 November 2021 14:25:31
```

## Deep Dive 

The `strftime` function dates back to the inception of C -- designed to provide maximum control over the format of the output date-time string. There are alternatives like `ctime` and `asctime`, but these don't offer the same level of customization.
  
In `strftime`, the format of the output string depends on the text within the third argument. Each character can be a format specifier starting with `%`, which instructs the method how to format the part of date-time.

For example, `%A` gives the full weekday name, `%d` the day of the month as a decimal number, and `%Y` the year.

Moreover, the function's implementation varies depending on the system's locale settings. The output can therefore change based on the configured language and region on the user's machine.

## See Also 

Here are few online resources to further explore the conversion of date to string in C:

- [C Library function - strftime()](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Date Time Formatting In C Programming](https://www.itrelease.com/2011/11/date-time-formatting-c-programming/)