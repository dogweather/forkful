---
title:    "C++ recipe: Getting the current date"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
In today's digital world, it is important for programmers to have accurate date and time information for various applications such as logging, scheduling, and data tracking. Knowing the current date and time is a fundamental aspect of programming and can be easily achieved using C++.

## How To
To get the current date in C++, we can use the `chrono` library. First, we need to include the `chrono` header file in our code. 

```
#include <chrono>
```

Then, we can use the `now()` function to get the current time as a `time_point` object. 

```
auto currentTime = std::chrono::system_clock::now();
```

To convert this `time_point` object into a readable format, we can use the `time_t` and `ctime` functions. 

```
std::time_t currentTime_t = std::chrono::system_clock::to_time_t(currentTime);
```

Using the `ctime` function, we can format the current time into a string and print it out. 

```
std::cout << "The current date and time is: " << std::ctime(&currentTime_t) << std::endl;
```

#### Sample Output
```
The current date and time is: Fri Jul 02 15:20:05 2021
```

## Deep Dive
Behind the scenes, the `chrono` library uses the `Duration` and `TimePoint` classes to represent time. The `now()` function returns a `TimePoint` object which is the current time since the system clock's epoch (January 1, 1970). This value is then converted into a `time_t` object, which is a representation of the current time in seconds since the epoch.

It is important to note that the `chrono` library also provides options to get the current time in other formats, such as `steady_clock` for consistent time intervals and `high_resolution_clock` for high-precision measurements.

## See Also
- [C++ Chrono Library Documentation](https://en.cppreference.com/w/cpp/chrono)
- [Working with Dates in C++: A Tutorial](https://www.learncpp.com/cpp-tutorial/89-working-with-dates-in-c/)
- [Current date and time in C++](https://stackoverflow.com/questions/997946/how-to-get-current-time-and-date-in-c)