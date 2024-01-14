---
title:                "C recipe: Calculating a date in the future or past"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating a date in the future or past can be a useful tool for a number of reasons. For example, you may need to schedule a task or event in the future or look back on a past event for reference. Whatever the reason may be, understanding how to calculate dates in your C program can greatly enhance its functionality.

## How To

Calculating dates in C can be done using the <time.h> library. First, you will need to initialize a structure of type "tm" which contains information about dates and times. Here is an example of how to do this:

```C
#include <stdio.h>
#include <time.h>

int main(){
    // initialize structure
    struct tm date;
    
    // set values for date
    date.tm_mon = 9;
    date.tm_mday = 10;
    date.tm_year = 2020;
    
    // calculate date in 10 days
    date.tm_mday += 10;
    
    // print new date
    printf("The new date is: %d/%d/%d\n",date.tm_mon,date.tm_mon,date.tm_year);
    
    return 0;
    
}
```

This code initializes a structure with a date of October 10th, 2020 and then calculates the date 10 days in the future. The output should be:

```
The new date is: 10/20/2020
```

You can also use the time() function to get the current time and date, and then use functions like localtime() and mktime() to manipulate the values. Here is an example:

```C
#include <stdio.h>
#include <time.h>

int main(){
    // initialize time variable
    time_t currentTime;
    
    // get current time
    time(&currentTime);
    
    // initialize time structure
    struct tm *local = localtime(&currentTime);
    
    // add 10 days to current date
    local->tm_mday += 10;
    
    // calculate new time
    time_t newTime = mktime(local);
    
    // print new time
    printf("The new time is: %s", ctime(&newTime));
    
    return 0;
}
```

This code takes the current time and adds 10 days to it, then prints out the new date. The output should be something like:

```
The new time is: Wed Oct 21 00:03:00 2020
```

## Deep Dive

Calculating dates in C may seem daunting at first, but understanding the underlying functions and structures can make it much easier. The <time.h> library has a variety of functions that can be used to manipulate dates and times. Some important ones to note are localtime(), which converts the time_t variable into a structure of type "tm", and mktime(), which does the opposite. The structure "tm" also has a number of variables that you can manipulate such as "tm_mon" for month, "tm_mday" for day, and "tm_year" for year.

Another important aspect to keep in mind is time zones. The functions mentioned above only take into account the local time, so if you want to calculate a date in a different time zone, you will need to adjust the values accordingly.

It's also worth noting that dates in C are represented as the number of seconds since January 1st, 1970 (also known as the Unix Epoch). This makes it easy to perform arithmetic operations on dates, but it's important to keep track of the units you are using (seconds, minutes, hours, etc.).

## See Also

- [Time and Date in C](https://www.programiz.com/c-programming/c-date-time)
- [C Programming Tutorial: Date & Time](https://www.youtube.com/watch?v=3mwwfD_N1mg)
- [C Language: Date and Time Functions](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)