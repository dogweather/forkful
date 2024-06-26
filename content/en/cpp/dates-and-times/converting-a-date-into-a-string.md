---
date: 2024-01-20 17:36:15.475116-07:00
description: 'How to: In modern C++, `<chrono>` and `<iomanip>` libraries are your
  friends for date-time operations. Here''s a quick method using `std::put_time`.'
lastmod: '2024-03-13T22:45:00.367870-06:00'
model: gpt-4-1106-preview
summary: In modern C++, `<chrono>` and `<iomanip>` libraries are your friends for
  date-time operations.
title: Converting a date into a string
weight: 28
---

## How to:
In modern C++, `<chrono>` and `<iomanip>` libraries are your friends for date-time operations. Here's a quick method using `std::put_time`:

```cpp
#include <iostream>
#include <iomanip>
#include <chrono>
#include <sstream>

int main() {
    auto now = std::chrono::system_clock::now(); // Get the current time
    auto time = std::chrono::system_clock::to_time_t(now); // Convert to time_t
    
    // Convert to tm struct for formatting
    std::tm tm = *std::localtime(&time);

    // String stream for output
    std::stringstream ss;

    ss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S"); // Format: YYYY-MM-DD HH:MM:SS

    std::string date_str = ss.str(); // Convert to string

    std::cout << date_str << std::endl; // Output the date string
    return 0;
}
```

Sample Output (depends on current date and time):
```
2023-03-15 14:25:30
```

## Deep Dive
Before `<chrono>` came into the picture, C++ programmers often had to wrestle with C-style time handling via `<ctime>`. This was less intuitive and more error-prone due to manual memory management and platform-dependent quirks.

Alternatives to `std::put_time` include using `strftime`, but that's more C-style. Third-party libraries like Boost.Date_Time can offer more functionality at the cost of adding dependencies.

A key implementation detail is understanding the format specifiers in `std::put_time`, which are similar to those used in `strftime`. You're mapping placeholders to date or time components — `%Y` for the full year, `%m` for the month, and so on.

## See Also
- [`<chrono>` documentation](https://en.cppreference.com/w/cpp/header/chrono)
- [`<iomanip>` documentation](https://en.cppreference.com/w/cpp/header/iomanip)
- [Boost.Date_Time](https://www.boost.org/doc/libs/release/libs/date_time/)
