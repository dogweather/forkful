---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:35:07.657841-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що і чому?

Parsing a date from a string means extracting the date components—day, month, year—from text. Programmers often do this to handle user input, store or manipulate dates, or interface with databases.

## How to:
## Як це зробити:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::string date_str = "2023-03-15";
    std::istringstream ss(date_str);
    std::tm date = {};
    
    ss >> std::get_time(&date, "%Y-%m-%d");
    if(ss.fail()) {
        std::cerr << "Failed to parse date." << std::endl;
    } else {
        std::cout << "Parsed date: "
                  << std::put_time(&date, "%d-%m-%Y") << std::endl;
    }
    
    return 0;
}
```

Sample output:
```
Parsed date: 15-03-2023
```

## Deep Dive:
## Поглиблений розбір:

Historically, C++ has relied on C-style strings and structs. The `<ctime>` header provides `struct tm` to represent time. The `std::get_time` and `std::put_time` functions, introduced in C++11, facilitate parsing and formatting.

Alternatives include using third-party libraries, like Boost.DateTime, or since C++20, the `<chrono>` library's more comprehensive date and time utilities.

Implementation-wise, understand that `std::get_time` uses format specifiers like `%Y` and `%d`. They match the expected form of the string. Tread carefully: if the string format doesn't match, parsing fails.

## See Also:
## Див. також:

- [C++ reference for std::get_time](https://en.cppreference.com/w/cpp/io/manip/get_time)
- [C++ reference for std::put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)
- [C++ reference for <chrono> library](https://en.cppreference.com/w/cpp/header/chrono)
- [Boost.DateTime documentation](https://www.boost.org/doc/libs/release/libs/date_time/)
