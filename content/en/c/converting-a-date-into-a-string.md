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

## What & Why?

Converting a date into a string means taking a date in the form of numbers and symbols and turning it into a more readable and understandable format for human users. Programmers do this for various reasons, such as displaying the date on a user interface, or storing it in a database in a standardized format.

## How to:

To convert a date into a string in C, we can use the `strftime()` function from the `time.h` library. This function takes in a format string and a `tm` struct, which holds the date and time information, and returns a string representation of the date in the specified format.

For example, let's say we have a `tm` struct representing the current date:

```C
tm currentDate; // assume this struct is initialized with the current date
```

To convert this date into a string in the format `MM/DD/YYYY`, we would use the `strftime()` function like this:

```C
char dateStr[11]; // create a character array with enough space to hold the formatted date
strftime(dateStr, sizeof(dateStr), "%m/%d/%Y", &currentDate); // convert the date and store it in the character array
printf("Current Date: %s", dateStr); // output the formatted date
```

The output would be: `Current Date: 07/23/2020`

## Deep Dive

Converting dates into strings has been a common task in programming since the early days. In the C language, the `strftime()` function was first introduced in the ANSI C standard in 1989. Prior to that, programmers had to manually format dates using various libraries and functions.

There are also alternative ways to convert dates into strings in C, such as using the `sprintf()` function, which works similarly to `printf()` but outputs the result to a string instead of the console. However, `strftime()` is the recommended method as it allows for more flexibility in formatting the date and avoids buffer overflows.

For programmers needing to work with time zones, the `asctime()` function can be useful for converting a `tm` struct into a string with the time zone information added.

## See Also

- [strftime() Function in C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [The History of C](https://www.techopedia.com/a-brief-history-of-the-c-programming-language/2/27778)
- [Alternative Methods to Convert Dates into Strings in C](https://www.geeksforgeeks.org/convert-date-string-using-strftime-c/)
- [tm Struct in C](https://www.geeksforgeeks.org/time-h-header-file-in-c-with-examples/)