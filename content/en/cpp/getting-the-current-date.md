---
title:                "C++ recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Why

In today's world, technology is an essential part of our daily lives. We use our devices for work, communication, and entertainment. One common task that many programs need to perform is getting the current date. Whether it's for data logging, time-sensitive tasks, or displaying the date to users, being able to get the current date is a fundamental skill in programming. In this blog post, we will explore how to get the current date in C++ and the different methods available.

## How To

To get the current date in C++, we can use the built-in `time` library. This library provides functions for manipulating and formatting date and time values. One of the functions we can use is the `time()` function. This function returns the current time in the form of the number of seconds that have passed since January 1, 1970, 00:00:00 UTC.

Let's see an example of how to use the `time()` function to get the current date and time.

```C++
#include <iostream>
#include <ctime>

int main() {
    // get the current time
    time_t now = time(0);

    // convert the time to a string
    char* current_time = ctime(&now);

    // print the current date and time
    std::cout << "Current Date and Time: " << current_time << std::endl;

    return 0;
}
```

Output:

```
Current Date and Time: Sat Aug 14 14:15:47 2021
```

We first include the `iostream` and `ctime` libraries, which provide the necessary functions for our code. Inside the `main()` function, we use the `time()` function to get the current time and store it in a variable called `now`. Then, we use the `ctime()` function to convert this time value to a string and store it in the `current_time` variable. Lastly, we print the current date and time using the `cout` statement.

There are also other functions available in the `time` library that can be used to manipulate and format date and time values, such as `localtime()` and `strftime()`. I encourage you to explore these functions and see what else you can do with dates and times in C++.

## Deep Dive

Under the hood, the `time` library uses the Unix epoch time system, which is the number of seconds that have elapsed since January 1, 1970, 00:00:00 UTC. This system was adopted to keep track of time in a uniform way across different systems and programming languages. It is also used in various other programming languages, such as Java, Python, and Perl.

While the `time()` function returns the time in the form of an integer, the `ctime()` function converts this value to a human-readable string using the standard date and time formatting. It is important to note that the `ctime()` function uses the local time zone of your system to display the date and time. If you want to display the date and time in a specific timezone, you can use the `localtime()` and `strftime()` functions.

## See Also

- [C++ `time` Library Documentation](https://www.cplusplus.com/reference/ctime/time/)
- [Unix Time - Wikipedia](https://en.wikipedia.org/wiki/Unix_time)