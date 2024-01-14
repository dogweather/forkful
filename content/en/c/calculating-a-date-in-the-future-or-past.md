---
title:    "C recipe: Calculating a date in the future or past"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Why

Have you ever needed to calculate a date in the future or past? Maybe you're trying to determine when a project will be completed, or when a bill is due. Whatever the reason may be, understanding how to calculate a date in the future or past can be a valuable skill in many programming projects. In this blog post, we will explore the basics of calculating dates in C.

# How To

To calculate a date in the future or past, we need to first understand how dates are represented in C. Dates are typically represented as a structure, with members for year, month, and day. We can use the `<time.h>` header file to work with dates in our C program.

Let's say we want to calculate a date 30 days from today. We can use the `time()` function to get the current date and time, and then use the `localtime()` function to convert the current time to a structure. Here's an example code:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t currentTime;
    struct tm *myDate;
    time(&currentTime);
    myDate = localtime(&currentTime);
    
    printf("Today's date is: %d/%d/%d\n", myDate->tm_mon + 1, myDate->tm_mday, myDate->tm_year + 1900);
    
    // Adding 30 days to the current date
    myDate->tm_mday += 30;
    mktime(myDate); // Re-normalize date
    
    printf("Date 30 days from today is: %d/%d/%d\n", myDate->tm_mon + 1, myDate->tm_mday, myDate->tm_year + 1900);

    return 0;
}
```

Output:

```
Today's date is: 12/3/2019
Date 30 days from today is: 1/2/2020
```

In this example, we use the `time()` function to get the current time in seconds since January 1, 1970. We then use the `localtime()` function to convert this time into a structure, which we can then manipulate. After adding 30 days to the `tm_mday` member, we use the `mktime()` function to re-normalize the date. This ensures our date is represented correctly, taking into account things like leap years.

We can also calculate dates in the past by subtracting days instead of adding them. Additionally, we can manipulate the `tm_mon` and `tm_year` members to calculate dates in different months and years.

# Deep Dive

Behind the scenes, C uses the Julian Day Number (JDN) system to represent dates. This is a continuous count of days from January 1, 4713 BC. By converting our desired date to a JDN, we can use simple arithmetic to calculate dates in the future or past.

The `mktime()` function also does much of the heavy lifting for us when re-normalizing a date. It takes into account things like daylight saving time and leap years, ensuring our date calculations are accurate.

# See Also

- [C Programming Language - Dates and Times](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [Julian Day Number](https://en.wikipedia.org/wiki/Julian_day)
- [mktime() function](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm)