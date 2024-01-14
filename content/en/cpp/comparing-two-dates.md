---
title:                "C++ recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may encounter situations where you need to compare two dates in your code. This could be when working with dates in a database, calculating time differences, or simply validating user input. Whatever the reason may be, having the ability to compare two dates is an important skill to have in your programming toolkit.

## How To

To compare two dates in C++, we can use the `std::chrono` library, which provides convenient functions for working with dates and times. Let's see how we can use this library to compare two dates.

````C++
#include <iostream>
#include <chrono>

int main() {
  // Create two datetime objects
  std::chrono::system_clock::time_point date1 = std::chrono::system_clock::now();
  std::chrono::system_clock::time_point date2 = std::chrono::system_clock::now();

  // Compare the two dates
  if (date1 == date2) {
    std::cout << "The two dates are equal" << std::endl;
  } else {
    std::cout << "The two dates are not equal" << std::endl;
  }

  return 0;
}
````

The `std::chrono` library allows us to create datetime objects using the `system_clock::now()` function, which returns the current date and time. We then use the `==` operator to compare the two dates, which returns a boolean value indicating whether the two dates are equal or not. In our example, we display a message to the user based on the comparison result. 

Let's take a look at another example where we compare two dates using the `std::chrono::duration` class, which allows us to work with time durations.

````C++
#include <iostream>
#include <chrono>

int main() {
  // Create two datetime objects
  std::chrono::system_clock::time_point date1 = std::chrono::system_clock::now();
  std::chrono::system_clock::time_point date2 = std::chrono::system_clock::now();

  // Calculate the duration between the two dates
  auto duration = date2 - date1;

  // Display the duration in seconds
  std::cout << "The duration in seconds is: " << std::chrono::duration_cast<std::chrono::seconds>(duration).count() << std::endl;

  return 0;
}
````

In this example, we use the `std::chrono::duration_cast` function to convert the duration between the two dates into seconds. We also use the `count()` function to get the actual value of the duration in seconds. This allows us to perform calculations and comparisons based on time durations.

## Deep Dive

Under the hood, the `std::chrono` library uses a system-specific clock to measure time. This ensures that the measurements are accurate and consistent across different platforms. Additionally, the library provides other useful functions for working with dates and times, such as `std::chrono::duration`, `std::chrono::time_point`, and `std::chrono::steady_clock`. These can be used in various scenarios depending on your specific needs.

When comparing two dates, it's important to also consider the time zone and daylight saving time. The `std::chrono` library allows you to specify the time zone for your datetime objects using the `std::chrono::zoned_time` class. This helps to ensure that your comparisons are accurate and account for any time zone differences.

## See Also

- [C++ Reference: std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [Tutorial: Using std::chrono in C++](https://www.geeksforgeeks.org/stdchrono-in-c/)
- [C++ Date and Time Library](https://howardhinnant.github.io/date/date.html)