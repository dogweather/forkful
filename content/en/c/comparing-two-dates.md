---
title:    "C recipe: Comparing two dates"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

In the realm of programming, we often find ourselves dealing with dates. Whether it's calculating an event's date or comparing two dates, it's crucial to have a solid understanding of date operations in order to create efficient and accurate programs. In this blog post, we will explore how to compare two dates in the C programming language.

## How To

The first step in comparing two dates in C is to convert both dates into a numerical format that can be easily compared. This can be achieved by using the `mktime()` function, which converts a structure containing date and time information into a time_t data type.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // defining first date
    struct tm first_date = { .tm_year=2020, .tm_mon=9, .tm_mday=10 };
    
    // converting first date to time_t data type
    time_t first_time = mktime(&first_date);
    
    // defining second date
    struct tm second_date = { .tm_year=2020, .tm_mon=9, .tm_mday=15 };
    
    // converting second date to time_t data type
    time_t second_time = mktime(&second_date);
    
    // comparing dates and printing output
    if (first_time < second_time) {
        printf("The first date is earlier than the second date.\n");
    }
    else if (first_time > second_time) {
        printf("The second date is earlier than the first date.\n");
    }
    else {
        printf("Both dates are the same.\n");
    }
    
    return 0;
}
```

Output:
```C
The first date is earlier than the second date.
```

## Deep Dive

In the above example, we used the `mktime()` function to convert the dates into time_t data types, which are essentially the number of seconds elapsed since January 1st, 1970. This format allows for easy comparison between two dates, as we can simply use comparison operators such as `<` or `>` to determine which date is earlier.

It's important to note that the `mktime()` function also takes into account the local time and timezone settings, so it's crucial to have these set correctly before using the function.

## See Also

For more information about date and time functions in C, please refer to the following resources:

- [time.h - C library reference](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [mktime() function in C](https://www.geeksforgeeks.org/time-h-mktime-function-in-c/)
- [Date and Time - C tutorial](https://www.programiz.com/c-programming/c-date-time)

Happy coding!