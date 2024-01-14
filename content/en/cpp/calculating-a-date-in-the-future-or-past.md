---
title:    "C++ recipe: Calculating a date in the future or past"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past may seem like a mundane task, but in the world of programming it can be a crucial function. Whether you're building a calendar app or working with time-sensitive data, being able to accurately determine future or past dates is essential.

## How To
To calculate a date in the future or past, we will be using the built-in "chrono" library in C++. This library provides a powerful set of tools for working with time and date values. Let's dive in with a simple example of adding 30 days to the current date:

```C++
#include <iostream>
#include <chrono>

int main() {
  // Get current date
  auto date = std::chrono::system_clock::now();
  
  // Add 30 days to current date
  auto futureDate = date + std::chrono::hours(30*24);
  
  // Convert to timepoint
  auto futureTP = std::chrono::system_clock::to_time_t(futureDate);
  
  // Print out calculated date
  std::cout << "Date in 30 days: " << std::put_time(std::localtime(&futureTP), "%F") << std::endl;
  
  return 0;
}
```

Output:

```
Date in 30 days: 2021-03-14
```

In this example, we first retrieve the current date using `std::chrono::system_clock::now()`. Then, we use the `+` operator to add a specific amount of time (in this case, 30 days) to the current date. The result is a variable of the same type as the original date, so we need to convert it to a timepoint using `std::chrono::system_clock::to_time_t()`. Finally, we use `std::put_time()` to format the new date in the desired format.

## Deep Dive
The "chrono" library in C++ provides a wide range of tools for working with time and date values, making it extremely versatile for various applications. Some of the most commonly used components of this library are:

- `duration`: Represents a specific duration of time in a specified unit.
- `time_point`: A specific point in time, represented by a duration from a specific starting point (usually January 1, 1970).
- `clock`: Defines the starting point used for calculating time points.
- `put_time`: Formatting function used to display date and time values in a specified format.

Using these components and their corresponding functions, we can easily perform complex calculations involving dates and times. It is important to familiarize yourself with the various aspects of the "chrono" library in order to effectively work with date and time values in your programs.

## See Also
- [C++ chrono library documentation](https://en.cppreference.com/w/cpp/chrono)
- [Using C++ chrono library for date and time calculations](https://www.programiz.com/cpp-programming/library-function/chrono)
- [How to handle dates and times in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)