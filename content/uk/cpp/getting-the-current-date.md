---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:13:21.390421-07:00
simple_title:         "Отримання поточної дати"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?
Getting the current date in C++ means grabbing the date from your system's clock. Programmers do this to timestamp events, measure performance, and for features like daily tasks or reminders.

## How to: / Як це зробити:
```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Get system's current time
    auto now = std::chrono::system_clock::now();
    // Convert to time_t for easy manipulation
    std::time_t time_now = std::chrono::system_clock::to_time_t(now);
    // Convert to human-readable form
    std::cout << std::ctime(&time_now);
    
    return 0;
}
```
Sample Output:
```
Mon Mar 27 22:31:57 2023
```

## Deep Dive / Поглиблений Аналіз:
- *Historical context*: Before C++11, programmers typically used `std::time()` and other C-style date and time functions. C++11 introduced `<chrono>` to handle time in a more type-safe and flexible way.
- *Alternatives*: Third-party libraries like Boost.Date_Time can also get the current date. Before `<chrono>`, this was a go-to solution for complex time operations.
- *Implementation details*: `<chrono>` abstracts time into durations and time points. Get the current time point using `system_clock`, and manipulate it with the time utilities in C++ Standard Library.

## See Also / Дивіться Також:
- [cppreference.com - <chrono>](https://en.cppreference.com/w/cpp/header/chrono)
- [cplusplus.com - Time tutorial](http://www.cplusplus.com/reference/ctime/)
- [ISO C++ Working Group - Date & Time proposal](https://cplusplus.github.io/LWG/lwg-active.html#998)
- [Boost.Date_Time documentation](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
