---
title:                "Calculating a date in the future or past"
html_title:           "C++ recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating past or future dates in programming involves determining a date that's a certain number of days, weeks, months, or years ahead or behind a given date. This operation is used in many practical applications such as reminders, scheduling events, or predicting future outcomes.

## How to:

In C++, the `<chrono>` library forms part of the solution, while the added functionality of the `<ctime>` library lets you manipulate the time and date specifically. Here's a simple code snippet to calculate a future date:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Get current time
    auto now = std::chrono::system_clock::now();
    // Add 30 days to the current time
    auto future = now + std::chrono::hours(24*30);

    // Convert timepoint to time_t
    std::time_t future_time = std::chrono::system_clock::to_time_t(future);
    
    std::cout << "Future date: " << std::ctime(&future_time);
}
```

The program will output something like:

```shell
Future date: Tue Sep 16 15:10:40 2025
```

## Deep Dive

Historically, date and time manipulation in C++ was not straightforward, leading to many errors and inconsistencies. The Standard Library's `std::chrono` (since C++11) introduced types that could handle duration, time points, clocks, etc., providing programmers with a powerful and accurate way to handle time.

As alternatives, libraries like `Boost.Date_Time` and `date.h` (or Howard Hinnant's date library) offer even more functionality. These libraries provide a more comprehensive API for date and time manipulation, including the handling of date, time, and timezone.

However, these alternatives may be overkill for simple tasks like calculating dates in the future or past. It's worth understanding how `std::chrono` works under the hood:

- `now()` function gets the current point in time (`time_point`).
- We can add any `duration` to this `time_point`, like `hours(24*30)`.
- We convert the `time_point` to `time_t`, a format which can be represented as a string.

## See Also

For more in-depth information about handling time in C++, consider the following resources:

1. [`std::chrono` documentation](http://en.cppreference.com/w/cpp/header/chrono)
2. [Boost Date Time Library](https://www.boost.org/doc/libs/1_68_0/doc/html/date_time.html)
3. [Howard Hinnant's date Library](https://howardhinnant.github.io/date/date.html)