---
title:    "C++ recipe: Getting the current date"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the current date while writing a program in C++? There are many reasons why you might need to access the current date, such as creating a timestamp for data logging, displaying the current date on a user interface, or scheduling events. In this blog post, we will explore how to get the current date in C++.

## How To

To get the current date in C++, we can use the `ctime` library. This library provides functions for dealing with date and time in C++. The `ctime` library contains the `time()` function, which returns the current time in seconds since the Unix Epoch (January 1st, 1970). We can use this time value to get the current date with the `localtime()` function, which converts the time into a structure containing the year, month, day, and other date information. Let's see an example of this in action:

```C++
#include <iostream>
#include <ctime>

int main() {

  // Get the current time in seconds
  time_t now = time(0);

  // Convert the time into a structure
  struct tm* current_time = localtime(&now);

  // Print the current date
  std::cout << "Current date: " << current_time->tm_year + 1900 << "-"
    << current_time->tm_mon + 1 << "-" << current_time->tm_mday << std::endl;

  return 0;
}
```

The output of this code will be the current date in the format of "YYYY-MM-DD". You can also format the date in any other way by accessing the corresponding members of the `current_time` structure.

## Deep Dive

The `time()` and `localtime()` functions use the system's clock to get the current date and time. However, this clock can be affected by changes in the system's time zone or by the user manually changing the system's clock. To ensure accuracy, we can use the `std::chrono` library, which provides a high-resolution clock that is not affected by system changes. Here is an example of getting the current date using `std::chrono`:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {

  // Get the current time using a high-resolution clock
  auto current_time = std::chrono::system_clock::now();

  // Convert the time to a time_t structure
  std::time_t now = std::chrono::system_clock::to_time_t(current_time);

  // Convert the time into a structure
  struct tm* date = std::localtime(&now);

  // Print the current date
  std::cout << "Current date: " << date->tm_year + 1900 << "-"
    << date->tm_mon + 1 << "-" << date->tm_mday << std::endl;

  return 0;
}
```

Using the `std::chrono` library ensures that we have the most accurate and up-to-date current date.

## See Also

- [C++ reference: ctime library](https://en.cppreference.com/w/cpp/header/ctime)
- [C++ reference: chrono library](https://en.cppreference.com/w/cpp/chrono)

Now you know how to get the current date in C++! Whether you need to display it for your users or use it for scheduling tasks, you can easily access the current date using the `ctime` and `std::chrono` libraries. Happy coding!