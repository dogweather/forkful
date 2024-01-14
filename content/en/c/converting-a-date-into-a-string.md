---
title:                "C recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may often come across situations where you need to convert a date into a string in your C programming projects. Whether it's for storing dates in a database or displaying them to users, there are many reasons why you would need to perform this conversion. In this blog post, we'll dive into the reasons behind it and learn how to accomplish it in C.

## How To

To convert a date into a string in C, we can use the `strftime()` function from the `<time.h>` library. This function takes in a format string and converts the given date and time into a string according to that format. Let's see an example:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm *local;
    char output[100];

    // Getting current time
    now = time(NULL);

    // Converting to local time
    local = localtime(&now);

    // Formatting date and time
    strftime(output, 100, "%d-%m-%Y %H:%M:%S", local);

    // Printing output
    printf("Today's date and time: %s\n", output);

    return 0;
}
```

In this example, we first declared a `time_t` variable to hold the current time and a `struct tm` variable to store the local time. Then, we used the `localtime()` function to convert the time into the local time zone. Finally, we used `strftime()` to format the date and time according to our desired format ("%d-%m-%Y %H:%M:%S") and stored it in the `output` variable. The output would be something like this:

```
Today's date and time: 24-09-2021 16:30:00
```

But what if we want to convert the date into a different format? In that case, we can change the format string accordingly. For example, if we want to display the weekday and month name in our output, we can use "%A, %B %d, %Y" as the format string.

## Deep Dive

Now that you have a basic understanding of how to convert a date into a string in C, let's dive deeper into the `strftime()` function. This function takes in three arguments: the output string pointer, the size of the output buffer, and the format string. The output string pointer is where the converted date and time will be stored, the size is the maximum number of characters that can be stored, and the format string specifies the desired output format.

The format string follows a specific syntax, with different format specifiers representing different parts of the date and time. For example, "%d" represents the day of the month, "%m" represents the month, "%Y" represents the full year, and so on. You can find a complete list of format specifiers and their meanings in the C documentation.

One important thing to note is that the `strftime()` function only works with `struct tm` variables, so if you're using a different variable type to store your date and time, you'll need to convert it to `struct tm` first.

## See Also

- [C strftime() function documentation](https://www.cplusplus.com/reference/ctime/strftime/)
- [C tutorial on converting date and time to string](https://www.programiz.com/c-programming/c-date-time-example)
- [Explanation of strftime() format specifiers](https://www.w3schools.in/c/date-time/)

In conclusion, converting a date into a string in C may seem like a small task, but it can come in handy in many different scenarios. With the `strftime()` function and some knowledge of date and time format specifiers, you can easily achieve this task in your C projects. Happy coding!