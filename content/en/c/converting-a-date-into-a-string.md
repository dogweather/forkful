---
title:                "Converting a date into a string"
html_title:           "C recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string is a basic yet essential task for any programmer. It allows for better manipulation and formatting of date data, which is commonly used in various applications such as scheduling, accounting, and data analysis.

## How To
To convert a date into a string in C, we can use the `strftime()` function from the `<time.h>` library. This function takes in a date and time value and converts it into a formatted string based on the provided format string.

To start, we first need to create a `struct tm` variable to hold our date and time information. This struct contains the individual components of a date and time, such as year, month, day, hour, minute, and second.

```
#include <stdio.h>
#include <time.h>

int main() {
    // Creating a struct to hold date and time information
    struct tm date = { .tm_year=2021, .tm_mon=8, .tm_mday=7,
                       .tm_hour=14, .tm_min=30, .tm_sec=0 };

    // Converting date into a string using strftime()
    char date_string[20];
    strftime(date_string, sizeof(date_string), "%Y/%m/%d %H:%M:%S", &date);

    printf("Date: %s\n", date_string); // Output: 2021/09/07 14:30:00

    return 0;
}
```

Here, we first create a `struct tm` variable named `date` and initialize it with the desired values using designated initializers. Then, we use the `strftime()` function to convert it into a string according to the specified format `%Y/%m/%d %H:%M:%S`, which translates to year/month/day hour:minute:second. The resulting string is stored in the `date_string` array and we can use `printf()` to display it.

## Deep Dive
The `strftime()` function provides great flexibility in formatting dates and times into strings. It takes in two arguments - the first one being the format string and the second one being a pointer to the `struct tm` containing the date and time data.

The format string is made up of two types of sequences: formatting sequences and literal characters. The formatting sequences start with a `%` symbol and are replaced with the corresponding value from the `struct tm` when the function is executed. Here are some common formatting sequences:

- `%Y`: year with century as a decimal number
- `%m`: month as a decimal number (01-12)
- `%d`: day of the month as a decimal number (01-31)
- `%H`: hour using a 24-hour clock as a decimal number (00-23)
- `%M`: minute as a decimal number (00-59)
- `%S`: second as a decimal number (00-59)

Apart from these, there are many more formatting sequences available for different date and time components. It is important to note that the order of the sequences in the format string is significant and must match the order of the components in the `struct tm`.

## See Also
- [Official C documentation for strftime()](https://en.cppreference.com/w/c/chrono/strftime)
- [Converting date to string in other programming languages](https://www.tutorialspoint.com/programming-language-conversion)