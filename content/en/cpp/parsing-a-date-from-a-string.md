---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:35:01.751968-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means converting text into a date data type. Programmers do it to handle date-related logic in a standardized, locale-independent manner, often for tasks like input validation, sorting, and storage.

## How to:
Use `<chrono>` and `<sstream>` to parse a date in C++. Here's a quick example:

```C++
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_text = "2023-04-01";
    std::istringstream ss(date_text);
    std::chrono::year_month_day parsed_date;
    
    ss >> std::chrono::parse("%F", parsed_date);
    if (ss.fail()) {
        std::cout << "Parse failed\n";
        return 1;
    }

    std::cout << "Year: " << int(parsed_date.year()) << '\n';
    std::cout << "Month: " << unsigned(parsed_date.month()) << '\n';
    std::cout << "Day: " << unsigned(parsed_date.day()) << '\n';

    return 0;
}
```

Sample Output:
```
Year: 2023
Month: 4
Day: 1
```

## Deep Dive
Parsing dates from strings isn't new. Back in the C days, `strptime` was typical. In modern C++, `<chrono>` is your friend. It neatly separates concerns: formatting/parsing with `std::chrono::parse`, and date manipulation with `std::chrono` types.

Before C++20, you'd likely reach for `std::get_time` or third-party libraries like Boost. Post-C++20, the standard library got a shiny upgrade with `std::chrono` improvements. Now you get type-safe date types and functions out-of-the-box.

The parsing function, `std::chrono::parse`, is versatile, understanding many date and time formats. The "%F" format we used above is ISO 8601 date format (year-month-day). But you can handle others too, just tweak the format string accordingly.

Remember, despite robust parsing, user input is tricky. Always handle parse errors gracefully, as done with `ss.fail()` in the example.

## See Also
Dive deeper into `<chrono>` with the official [cppreference page](https://en.cppreference.com/w/cpp/header/chrono).

Get historical context from Stroustrup's take on C++ history at [The Design and Evolution of C++](http://www.stroustrup.com/hopl2.pdf).

For edge cases or non-standard formats, consider checking out [Boost.DateTime](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html).
