---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates in programming refers to determining the order of two distinct dates or calculating the difference between them. Programmers do this for various reasons, such as sorting events by date, calculating the age of users, and determining the duration of events.

## How to:

Here's a simple comparison in C++ using the `std::chrono::system_clock::time_point` type.

```C++
#include <chrono>
#include <iostream>

int main() {
    // get current time
    auto now = std::chrono::system_clock::now();
    
    // create another time point 1 hour later
    auto later = now + std::chrono::hours(1);
    
    // comparison
    if (later > now) {
        std::cout << "later is greater than now\n";
    } else {
        std::cout << "later is not greater than now\n";
    }
    
    return 0;
}
```

The output here would be `later is greater than now` since we specifically created `later` to be 1 hour ahead of `now`.

## Deep Dive:

### Historical Context
Back in the days of C, date/time comparisons involved a lot of manual work, with programmers having to individually compare the year, month, day, etc., of date structures. In comparison, modern C++ libraries, like Chrono, simplify these operations significantly.

### Alternatives
There are several ways you could compare dates in C++. You could use boost's date_time library if you need more functions or deal with unusual calendar systems. However, if you need just the basics, using std::chrono is often good enough and requires no additional dependencies.

### Implementation Details
When comparing two dates using std::chrono, keep in mind that it essentially compares the time since UNIX epoch (1 January 1970). This makes it easy to calculate differences between dates. However, it does not take into account issues like time zonal differences, daylight savings, or leap seconds. For handling these complexities, libraries such as boost date_time or Howard Hinnant's date library are worth considering.

## See Also:

- [C++ std::chrono library documentation](https://en.cppreference.com/w/cpp/chrono)
- [Boost date_time library documentation](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [Howard Hinnant's date library on GitHub](https://github.com/HowardHinnant/date)