---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the act of taking text input and converting it into a date data type. Programmers often do this to accurately manage and manipulate date-related information from different text sources like data files or user input.

## How to:

C++ offers a powerful `<chrono>` library which includes utilities for dealing with dates. To parse a string to date, we may use `from_stream` method from the `date::parse` class, available since C++20.

```C++
#include <chrono>
#include <sstream>

int main()
{
    std::istringstream stream{"2022-04-01"};
    std::chrono::year_month_day date;
    stream >> std::chrono::parse("%Y-%m-%d", date);
    std::cout << date << '\n'; // Output: 2022-04-01
}
```

Make sure that the date format in the string matches the format specified in the parse function.

## Deep Dive

**Historical context:** Parsing dates from strings has evolved over time. In C++98 and C++03, we had to manage this task manually or via third-party libraries like Boost. With the introduction of the `<chrono>` library in C++11 and subsequent enhancements in later versions, dealing with time and date has become far easier.

**Alternatives:** Prior to C++20, or if your compiler doesn't support `<chrono>` utilities, you could use `strftime` and `strptime` functions from `<ctime>`, or opt for the Boost.Date_Time library.

```C++
// Using strptime from <ctime>
#include <ctime>
#include <iostream>

int main()
{
    struct tm tm = {0};
    std::string str = "2022-04-01";
    
    strptime(str.c_str(), "%Y-%m-%d", &tm);
    time_t time = mktime(&tm);
    
    std::cout << ctime(&time);  // Output: Fri Apr  1 00:00:00 2022
}
```

**Implementation details:** `std::chrono::parse` reads the date and stores it in the `std::chrono::year_month_day` object. Understanding the way in which C++ handles time and date might seem complicated at first, but ultimately serves to provide strong type safety and precision.

## See Also:

- [C++ `<chrono>`](https://en.cppreference.com/w/cpp/header/chrono)
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)