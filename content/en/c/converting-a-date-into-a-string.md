---
title:    "C recipe: Converting a date into a string"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why
Have you ever needed to display a date in a string format? Maybe you want to print the date in a specific order or include it in a message. Whatever the reason may be, converting a date into a string is a useful skill to have in your C programming arsenal.

## How To
To convert a date into a string, we need to use a combination of C library functions including `strftime()` and `time()`. Let's take a look at a code snippet to better understand the process:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // create a struct tm instance to store the date and time
    struct tm date;
    // use time() function to get current time
    time_t now = time(NULL);
    // convert time to local time and store in date
    localtime_s(&date, &now);
    
    // use strftime() function to format date
    char output[20];
    strftime(output, sizeof(output), "%b %d, %Y", &date);
    
    // print the formatted date string
    printf("Today's date is: %s", output);
    
    return 0;
}
```

In this code, we first create a struct `tm` instance to store the date and time. We then use the `time()` function to get the current time and convert it to local time using the `localtime_s()` function. Next, we use the `strftime()` function to format the date according to our desired format. In this case, the format string `"%b %d, %Y"` represents the abbreviated month name, day of the month, and full year. Finally, we print the formatted date string using the `printf()` function.

Running this code will produce an output similar to the following:

```
Today's date is: Aug 20, 2021
```

By playing around with the format string and the `strftime()` function, you can customize the output of your date string to fit your needs.

## Deep Dive
Converting a date into a string requires a deep understanding of C library functions and how they handle date and time. The `time()` function, for example, returns the current time in seconds since 00:00:00 on January 1, 1970, known as the Unix epoch. This value is stored in a `time_t` variable and can be converted into human-readable formats using functions such as `localtime()` and `gmtime()`. The `strftime()` function then takes this converted time and formats it according to the provided format string.

It is important to note that the `strftime()` and `localtime()` functions rely on the system's current locale setting to determine the appropriate date and time format. Therefore, it is vital to ensure that the locale setting is correct and in line with how you want your date to be displayed.

## See Also
- [C strftime() function documentation](https://www.cplusplus.com/reference/ctime/strftime/)
- [C time() function documentation](https://www.cplusplus.com/reference/ctime/time/)
- [C localtime() function documentation](https://www.cplusplus.com/reference/ctime/localtime/)