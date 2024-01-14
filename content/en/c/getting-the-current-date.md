---
title:    "C recipe: Getting the current date"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why
Have you ever wondered how your computer knows the current date and time? Or have you ever needed to use the current date in your programming project? Getting the current date may seem like a small task, but it can be very useful for various applications. In this blog post, we will explore how to get the current date in a C program.

## How To

To get the current date in a C program, we will use the <time.h> header file. This header file contains functions for manipulating date and time information. The main function we will be using is the `time()` function. Here is an example code snippet showing how to get the current time and date:

```C
#include <stdio.h>
#include <time.h>
  
int main()
{
    // declaring variables to store current time
    time_t rawtime; 
    struct tm * timeinfo;
  
    // getting current time and date
    time (&rawtime);
    timeinfo = localtime (&rawtime);
  
    // printing current date and time
    printf("Current date and time: %s", asctime(timeinfo));
  
    return 0;
}
```

Running this code will output the current date and time in the following format:

`Current date and time: Wed Jul 28 11:59:53 2021`

The `time()` function returns the current time in seconds since January 1, 1970, also known as the "Unix Epoch." We then use the `localtime()` function to convert this time into a more readable format.

## Deep Dive
As mentioned earlier, the `time()` function returns the time in seconds since the Unix Epoch. This value is often referred to as a "timestamp." We can use this timestamp to perform various operations, such as calculating the time difference between two dates.

The `localtime()` function takes the timestamp and converts it into a `struct tm` type, which contains all the necessary information about the current date and time, such as year, month, day, hour, minute, and second. This makes it easy to access and manipulate specific components of the current date and time.

If you want more control over the output format, you can use the `strftime()` function to format the time and date according to your needs. This function takes in a format string and the `struct tm` type and returns a string with the formatted date and time. You can find a list of formatting options for the format string in the `strftime()` function documentation.

## See Also
- <a href="https://www.geeksforgeeks.org/time-function-in-c/" target="_blank">GeeksforGeeks - time() function in C</a>
- <a href="https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm" target="_blank">Tutorials Point - localtime() function in C</a>
- <a href="https://www.programiz.com/c-programming/library-function/time/strftime" target="_blank">Programiz - strftime() function in C</a>

Getting the current date and time may seem like a small and trivial task, but it can be very useful in various programming applications. Understanding how to get the current date in C can open up a world of possibilities for your coding projects. So why not give it a try and see what you can create with this knowledge? Happy coding!