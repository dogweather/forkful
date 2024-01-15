---
title:                "Comparing two dates"
html_title:           "C++ recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates is a common operation when working with dates in programming, whether it's to determine if one date is before or after another or to calculate the difference between them. It allows developers to perform various tasks such as creating reminders, scheduling events, and sorting data based on date.

## How To

To compare two dates in C++, we can use the `std::chrono` library. This library provides a high-precision clock that can measure time in different units, including seconds, milliseconds, microseconds, and nanoseconds. We can use this library to create `std::chrono::time_point` objects that represent a specific point in time.

```C++
// include necessary libraries
#include <iostream>
#include <chrono> 

int main()
{
  // create two time_point objects representing two different dates
  std::chrono::time_point start = std::chrono::system_clock::now();
  std::chrono::time_point end = std::chrono::system_clock::now();

  // compare the two dates using the comparison operators (>, <, >=, <=)
  if (start > end) {
    std::cout << "Start date is after end date";
  } else if (start < end) {
    std::cout << "Start date is before end date";
  } else {
    std::cout << "Start date is the same as end date";
  }

  // calculate the duration between the two dates in days
  std::chrono::duration<int> diff = end - start;
  std::cout << "Duration between the two dates is " << diff.count() << " days";

  return 0;
}
```

The output of the above code would be:
```
Start date is before end date
Duration between the two dates is 0 days
```

## Deep Dive

To compare two dates, we first need to understand how dates are represented in the `std::chrono` library. The `std::chrono::time_point` class represents a point in time, and it takes two template parameters: `Clock` and `Duration`. The `Clock` parameter determines the time source used to measure the time and is typically one of the three clocks: `std::chrono::system_clock`, `std::chrono::steady_clock`, or `std::chrono::high_resolution_clock`. The `Duration` parameter represents the unit of time used to measure the time and is typically one of the `std::chrono::duration` classes (e.g. `std::chrono::seconds`, `std::chrono::microseconds`). 

When we create a `std::chrono::time_point` object, it is initialized with the current time by default. However, we can also specify a specific time to initialize the object, as shown in the code example above.

To compare two `std::chrono::time_point` objects, we can use the comparison operators (`>`, `<`, `>=`, `<=`) provided by the `std::chrono` library. These operators compare the two time points based on the specified clock's resolution. In the above code, we used `std::chrono::system_clock` as the clock, which has a resolution of one second.

Apart from comparing two dates, the `std::chrono` library also allows us to perform arithmetic operations on time points, such as adding or subtracting a specified duration. This can be useful when calculating the difference between two dates or adding a specific amount of time to a date.

## See Also

- [Comparing Dates and Times in C++](https://www.geeksforgeeks.org/comparing-dates-and-times-in-c/)
- [std::chrono Library Reference](https://en.cppreference.com/w/cpp/chrono)
- [Computing Dates in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)