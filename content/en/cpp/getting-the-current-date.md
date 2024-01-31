---
title:                "Getting the current date"
date:                  2024-01-20T15:13:11.856840-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Grabbing the current date in your C++ program can be handy: think logging, timestamps, or scheduling features. It's about staying relevant to the now - your software knows today, just as well as you do.

## How to:
Here's how to fetch the current date with `<chrono>`—modern, clean, no-nonsense.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Get current system time
    auto now = std::chrono::system_clock::now();

    // Convert to time_t, then to tm for a readable format
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);
    std::tm* now_tm = std::localtime(&now_c);

    // Print in YYYY-MM-DD format
    std::cout << (now_tm->tm_year + 1900) << '-' 
              << (now_tm->tm_mon + 1) << '-'
              <<  now_tm->tm_mday << '\n';

    return 0;
}
```

Sample output you'd get today:
```
2023-4-14
```
Not fancy, gets the job done. 

## Deep Dive
Back in the day, C time functions ruled—`<ctime>` was your go-to. But with C++11 and later, `<chrono>` took the spotlight. It's type-safe and avoids common mistakes with old-school C functions.

Alternatives? Sure. You could use old `std::time` or even OS-specific APIs if you like living on the edge (or have very specific needs).

And implementation details? `<chrono>` represents time points, durations, and clocks. It's precise and carefully designed. Time is tricky (leap seconds, time zones), and `<chrono>` handles this complexity under the hood, so you don't have to sweat it.

## See Also
- [C++ Reference - `<chrono>` library](https://en.cppreference.com/w/cpp/chrono)
- [C++ Reference - Old-school `<ctime>`](https://en.cppreference.com/w/cpp/header/ctime)
- For a bigger deep-dive, check out Howard Hinnant's date library, a `<chrono>` extension: [https://github.com/HowardHinnant/date](https://github.com/HowardHinnant/date)
- If you ever need timezone support right out of the box, try this: [https://en.cppreference.com/w/cpp/chrono/current_zone](https://en.cppreference.com/w/cpp/chrono/current_zone)
