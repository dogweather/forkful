---
date: 2024-02-03 19:02:47.219646-07:00
description: "How to: C++ provides several ways to get the current date, including\
  \ the C++ standard library and third-party libraries like Boost. The following examples\u2026"
lastmod: '2024-03-13T22:45:00.366994-06:00'
model: gpt-4-0125-preview
summary: C++ provides several ways to get the current date, including the C++ standard
  library and third-party libraries like Boost.
title: Getting the current date
weight: 29
---

## How to:
C++ provides several ways to get the current date, including the C++ standard library and third-party libraries like Boost. The following examples demonstrate how to accomplish this task.

### Using `<chrono>` (C++20 and later)
C++20 introduced more functionalities in the `<chrono>` library, making it straightforward to get the current date:
```cpp
#include <iostream>
#include <chrono>
#include <format> // For std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Capture the current time
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Convert to time_t

    // Format the time to a readable format
    std::cout << "Current Date: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Sample Output:**
```plaintext
Current Date: 2023-03-15
```

### Using `<ctime>`
For programmers working with older versions of C++ or those who prefer the traditional C library:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Get current time
    std::tm* now = std::localtime(&t);
    std::cout << "Current Date: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**Sample Output:**
```plaintext
Current Date: 2023-03-15
```

### Using Boost Date_Time
For projects that utilize the Boost libraries, the Boost Date_Time library offers an alternative method to get the current date:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Get the current day using Boost's Gregorian calendar
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "Current Date: " << today << std::endl;

    return 0;
}
```
**Sample Output:**
```plaintext
Current Date: 2023-Mar-15
```
These examples provide a basic foundation for working with dates in C++, crucial for a wide range of applications.
