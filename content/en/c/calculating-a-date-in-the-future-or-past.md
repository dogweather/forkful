---
title:    "C recipe: Calculating a date in the future or past"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to calculate a date in the future or past? Maybe you're working on a project that requires generating a schedule, or you just want to know when your next birthday will fall on a weekend. Regardless of the reason, knowing how to calculate a date in the future or past can be a useful skill to have in your coding arsenal.

## How To

To calculate a date in the future or past, we first need to understand how dates are represented in programming. In C, dates are stored as a number representing the number of seconds elapsed since January 1, 1970. This value is commonly known as "unix time."

To calculate a date, we need to first convert our desired date into unix time. This can be done using the `mktime()` function. Let's say we want to calculate a date 5 days in the future from today. We can use the following code:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // get current time
    time_t now = time(NULL);

    // define a struct to hold the desired date
    struct tm future;

    // add 5 days to current time
    future = *localtime(&now);
    future.tm_mday += 5;

    // convert struct to unix time
    time_t future_time = mktime(&future);

    // print the date
    printf("The date 5 days from now will be: %s", ctime(&future_time));

    return 0;
}
```

This code first gets the current time and then uses the `localtime()` function to convert it into a `struct tm` object. We then add 5 days to the `tm_mday` field, which represents the day of the month, and use the `mktime()` function to convert the struct back into unix time. Finally, we print out the calculated date using the `ctime()` function.

Running this code will give us the following output:

```C
The date 5 days from now will be: Wed Oct 20 12:34:56 2021
```

Similarly, we can also calculate a date in the past by subtracting days from the current date.

## Deep Dive

While the above example is a simple way to calculate a date, it's important to note that it does not take into account things like leap years or different time zones. To handle these cases, we can use the `gmtime()` and `localtime()` functions, which handle time zones differently.

For a more comprehensive way to handle dates, we can use the `strptime()` function, which allows us to specify a date in a particular format and retrieve the unix time for that date. This gives us more control over how we calculate dates and can be especially useful when working with dates in different formats.

## See Also

- [mktime() function](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm)
- [gmtime() function](https://www.tutorialspoint.com/c_standard_library/c_function_gmtime.htm)
- [localtime() function](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [strptime() function](https://www.tutorialspoint.com/c_standard_library/c_function_strptime.htm)