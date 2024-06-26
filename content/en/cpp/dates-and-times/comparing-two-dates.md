---
date: 2024-01-20 17:32:32.259273-07:00
description: 'How to: C++ makes life easy with the `<chrono>` header.'
lastmod: '2024-03-13T22:45:00.368708-06:00'
model: gpt-4-1106-preview
summary: C++ makes life easy with the `<chrono>` header.
title: Comparing two dates
weight: 27
---

## How to:
C++ makes life easy with the `<chrono>` header.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std::chrono;

    // Create system_clock time points
    system_clock::time_point today = system_clock::now();
    system_clock::time_point someDay = system_clock::now() - hours(24); // Yesterday

    // Convert to time_t for comparison
    time_t today_time_t = system_clock::to_time_t(today);
    time_t someDay_time_t = system_clock::to_time_t(someDay);

    if (today_time_t > someDay_time_t) {
        std::cout << "Today is after someDay.\n";
    } else if (today_time_t < someDay_time_t) {
        std::cout << "Today is before someDay.\n";
    } else {
        std::cout << "Dates are the same.\n";
    }

    return 0;
}
```

Sample output:

```
Today is after someDay.
```

## Deep Dive:
Since C++11, `<chrono>` is where it's at for date and time. Before that, you were likely wrestling with `<ctime>` and structs like `tm`. Not pretty.

Alternatives? Sure, there's third-party libraries like Boost.DateTime. But why complicate when `<chrono>` is right there and evolving.

Implementation details to keep in your back pocket:
- `std::chrono` deals with time points and durations.
- `system_clock` measures real-world time.
- `time_point` is a specific point in time (e.g., a date).
- `time_t` is an arithmetic type, handy for comparisons.

## See Also:
- C++ Reference for `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Comparison of date and time libraries: http://www.boost.org/doc/libs/1_64_0/doc/html/date_time.html
- Good ol’ `<ctime>`, if you're feeling nostalgic or masochistic: https://en.cppreference.com/w/cpp/header/ctime
