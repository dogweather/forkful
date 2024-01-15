---
title:                "भविष्य या भूतकाल में एक दिन की गणना"
html_title:           "C: भविष्य या भूतकाल में एक दिन की गणना"
simple_title:         "भविष्य या भूतकाल में एक दिन की गणना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Kabhi kabhi hume pehle ya baad ki date nikalni hoti hai. Isse hum jan sakte hai ke kis din konsa kaam karna hai ya kis din koi event hai. C programming mein, date calculation ka feature hai jo hume future ya past dates nikalne mein madad karta hai.

## How To

Calculating date in the future or past involves using the "time.h" header file and various functions like "mktime()" and "localtime()". Let's see an example of calculating a date 100 days in the future:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t now, future;
    struct tm *timeInfo;

    time(&now); // get current time
    future = now + (100 * 24 * 60 * 60); // add 100 days

    timeInfo = localtime(&future); // convert to readable format

    printf("Date 100 days from now: %s", asctime(timeInfo)); // print date

    return 0;
}
```
Output:
```
Date 100 days from now: Wed Nov 17 23:18:22 2021
```

Similarly, we can calculate dates in the past by subtracting days, months or years from the current date. Just remember to convert to readable format using "localtime()" before printing the result.

## Deep Dive

The "time.h" header file contains various functions and structures for handling date and time in C. The "mktime()" function takes in a structure containing date and time information and converts it to a time_t value, which represents the number of seconds elapsed since January 1, 1970. This value can then be manipulated using arithmetic operations to calculate future or past dates.

The "localtime()" function converts the time_t value back to a readable structure containing date and time information. This can be further formatted using functions like "asctime()" to print it in a human-readable format.

## See Also

- [C Programming - Date & Time functions](https://www.programiz.com/c-programming/library-function/time)
- [time.h - C++ Reference](https://www.cplusplus.com/reference/ctime/)