---
date: 2024-01-20 17:28:31.987934-07:00
description: "Calculating a date in the future or past means figuring out what date\
  \ it'll be after or before a certain time span. It's useful for creating reminders,\u2026"
lastmod: '2024-03-13T22:45:00.369613-06:00'
model: gpt-4-1106-preview
summary: "Calculating a date in the future or past means figuring out what date it'll\
  \ be after or before a certain time span. It's useful for creating reminders,\u2026"
title: Calculating a date in the future or past
weight: 26
---

## What & Why?
Calculating a date in the future or past means figuring out what date it'll be after or before a certain time span. It's useful for creating reminders, setting expiry dates, scheduling events, or simply logging how much time has passed.

## How to:
C++20 introduced the `<chrono>` library upgrades, so dealing with time is less of a hassle. Here's a quick example of adding days to the current date:

```C++
#include <iostream>
#include <chrono>
#include <format>

using namespace std::chrono;

int main() {
    // Get today's date
    auto today = floor<days>(system_clock::now());
    
    // Add 30 days to today
    auto future_date = today + days(30);
    
    // Convert to time_point to output using system_clock
    auto tp = system_clock::time_point(future_date);
    
    // Output
    std::cout << "Today's date: "
              << std::format("{:%F}\n", today);
    std::cout << "Future date (30 days later): "
              << std::format("{:%F}\n", tp);
    return 0;
}
```

Sample output:
```
Today's date: 2023-03-15
Future date (30 days later): 2023-04-14
```

Subtracting days works similarly—you'd just use `-` instead of `+`.

## Deep Dive
Before C++20, you'd maybe use a library like Boost to manipulate dates. But the updated `<chrono>` simplifies it with `system_clock`, `year_month_day`, and `duration` types.

Historically, calculating dates was complex due to manual handling of varying month lengths, leap years, and time zones. C++20's `<chrono>` addresses these by providing calendar and timezone support.

Alternatives? You could still use Boost or even handcraft your own date logic (adventurous, but why?). There's also third-party libraries like Howard Hinnant's "date" library, which was influential in the C++20 chrono updates.

Implementation-wise, `<chrono>` defines durations as compile-time rational constants, avoiding floating-point issues. Types like `year_month_day` rest on `sys_days`, which represents a time_point as days since a common epoch (1970-01-01).

## See Also
- C++ Reference for `chrono`: https://en.cppreference.com/w/cpp/header/chrono
- Howard Hinnant's Date Library (a precursor to C++20's chrono updates): https://github.com/HowardHinnant/date
- Boost Date/Time documentation: https://www.boost.org/doc/libs/release/libs/date_time/
