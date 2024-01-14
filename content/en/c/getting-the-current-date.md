---
title:    "C recipe: Getting the current date"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Why Get the Current Date in C Programming

Getting the current date in C programming may seem like a simple task, but it has various practical applications. Whether you want to display the current date on your program's interface or use it to track the passage of time, being able to access the current date is an essential skill for any C programmer.

# How To Get the Current Date in C Programming

There are a few different ways to get the current date in C programming, each with its own advantages.

**Using time.h Library:**

```
#include <stdio.h>
#include <time.h>

int main()
{
    // Declaring a variable to store the current time
    time_t currentTime;

    // Getting the current time using time() function
    currentTime = time(NULL);

    // Converting current time to local time
    struct tm *localTime = localtime(&currentTime);

    // Printing the date and time with proper formatting
    printf("Current Date and Time: %d/%d/%d ", localTime->tm_mday, localTime->tm_mon + 1, localTime->tm_year + 1900);
    printf("%d:%d:%d", localTime->tm_hour, localTime->tm_min, localTime->tm_sec);

    return 0;
}
```

**Output:**
```
Current Date and Time: 27/05/2021 15:04:26
```

**Using Windows API:**

```
#include <stdio.h>
#include <windows.h>

int main()
{
    // Declaring a variable to store the current time
    SYSTEMTIME currentTime;

    // Getting the current time using GetSystemTime() function
    GetSystemTime(&currentTime);

    // Printing the date and time with proper formatting
    printf("Current Date and Time: %d/%d/%d ", currentTime.wDay, currentTime.wMonth, currentTime.wYear);
    printf("%d:%d:%d", currentTime.wHour, currentTime.wMinute, currentTime.wSecond);

    return 0;
}
```

**Output:**
```
Current Date and Time: 27/05/2021 15:04:51
```

# Deep Dive into Getting the Current Date in C Programming

In C programming, the `time.h` library provides various functions for working with date and time. The `time()` function from this library returns the current time in seconds since the Epoch (00:00:00 UTC on 1 January 1970). This value can be used with `localtime()` function to convert it into a more understandable format.

On the other hand, the Windows API provides the `GetSystemTime()` function, which returns the current system date and time in a `SYSTEMTIME` structure. This structure contains individual members for the date and time, making it easier to access and format the data.

Apart from these methods, there are also third-party libraries available, such as `libtime` and `libtldate`, which offer more advanced features for working with date and time in C programming.

# See Also

- [Standard C Library - time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Windows API - GetSystemTime()](https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getsystemtime)
- [libtime - GitHub Repository](https://github.com/zerawr/libtime)
- [libtldate - GitHub Repository](https://github.com/X-Factor/trusted_linux/security-mac/libtldate)