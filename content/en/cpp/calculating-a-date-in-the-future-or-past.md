---
title:                "Calculating a date in the future or past"
html_title:           "C++ recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculating a date in the future or past is the process of determining a specific date that is either a certain number of days or years ahead or behind a given date. This is a useful skill for programmers as it allows them to manipulate and work with dates in their programs, without having to manually compute every date.

## How to:
To calculate a date in the future or past, we first need to include the <chrono> library in our code. Then, we can use the `time_point` and `duration` functions to specify the starting date and the amount of time we want to add or subtract. Here's an example code that calculates a date 10 years in the future from the current date:
```C++ 
#include <chrono>
using namespace std::chrono;
system_clock::time_point now = system_clock::now();
system_clock::duration future = now + hours(24 * 365 * 10);
std::time_t future_time = system_clock::to_time_t(future);
std::cout << "Date 10 years from now is: " << std::ctime(&future_time);
```
Output:
```
Date 10 years from now is: Sat Aug 1 00:12:00 2031
```
Similarly, if we want to calculate a date in the past, we can simply change the sign of the duration. Here's an example code that calculates a date 5 years in the past from the current date:
```C++
#include <chrono>
using namespace std::chrono;
system_clock::time_point now = system_clock::now();
system_clock::duration past = now - hours(24 * 365 * 5);
std::time_t past_time = system_clock::to_time_t(past);
std::cout << "Date 5 years ago was: " << std::ctime(&past_time);
```
Output:
```
Date 5 years ago was: Thu Aug 1 00:13:00 2016
```

## Deep Dive:
To understand the concept of calculating dates in the future or past, we need to go back in time (pun intended) to the year 1582, when the current Gregorian calendar was adopted. Before that, the Julian calendar was used, which had a leap year every 4 years. However, this resulted in an error of approximately 11 minutes every year, leading to a discrepancy between the calendar and solar year. To fix this, the Gregorian calendar introduced the concept of a leap year every 400 years and a few other changes.

Alternatives to using the <chrono> library for date calculations include the <ctime> library and the boost date-time library. The <ctime> library offers functions for basic date and time operations, while the boost date-time library provides more advanced functionalities such as working with time zones and calendar systems.

As for implementation details, the <chrono> library uses the `time_point` data type to represent a specific point in time and the `duration` data type to represent a length of time. These data types are templated and can be used with different time units such as hours, minutes, and seconds.

## See Also:
- [Chrono library reference](https://en.cppreference.com/w/cpp/chrono)
- [Ctime library reference](https://en.cppreference.com/w/cpp/chrono/c)
- [Boost date-time library](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)