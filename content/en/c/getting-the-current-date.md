---
title:                "Getting the current date"
html_title:           "C recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Many programs require accessing the current date and time for various purposes such as timestamping events, scheduling tasks, or displaying the current date in a user-friendly format.

## How To
To get the current date and time in C, we will use the `time.h` library. First, we need to declare a variable of type `time_t` which stores the number of seconds since January 1, 1970. Then, we can use the `time()` function to retrieve the current time and store it in our declared variable. Finally, we can use `ctime()` to convert the time value into a string and print it out.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // declare a variable of type time_t
    time_t currentTime; 
    
    // use the time() function to retrieve the current time
    time(&currentTime); 
    
    // use ctime() to convert the time value into a string
    char* date = ctime(&currentTime); 
    
    // print out the current date and time
    printf("The current date and time is %s", date); 
    
    return 0;
}
```

### Sample Output
```
The current date and time is Tue Aug 17 09:27:41 2021
```

## Deep Dive
The `time()` function returns the number of seconds since January 1, 1970 which is called "Epoch time". This is the standard way of representing time in modern computing systems. However, some operating systems may have a different Epoch time or may not support it at all. In those cases, the `time()` function may return -1 and the current time will be unavailable.

Furthermore, the `ctime()` function converts the time value into a string using the local time zone and adds a newline character at the end. This means that the displayed time may vary depending on the local time zone settings. To avoid this, we can use the `gmtime()` function instead which converts the time into a UTC time.

## See Also
- [C time() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [C ctime() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_ctime.htm)
- [C gmtime() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_gmtime.htm)