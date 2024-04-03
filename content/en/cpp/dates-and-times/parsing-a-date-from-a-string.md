---
date: 2024-02-03 19:02:42.844039-07:00
description: "How to: In modern C++, you can use the `<chrono>` library to handle\
  \ dates and times natively, but it doesn't directly support parsing from strings\
  \ without\u2026"
lastmod: '2024-03-13T22:45:00.366106-06:00'
model: gpt-4-0125-preview
summary: In modern C++, you can use the `<chrono>` library to handle dates and times
  natively, but it doesn't directly support parsing from strings without manual parsing
  for more complex formats.
title: Parsing a date from a string
weight: 30
---

## How to:
In modern C++, you can use the `<chrono>` library to handle dates and times natively, but it doesn't directly support parsing from strings without manual parsing for more complex formats. However, for ISO 8601 date formats and simple custom formats, here's how you can accomplish parsing.

**Using `<chrono>` and `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601 format
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Parsed date: " << parsed_date << std::endl;
    } else {
        std::cout << "Failed to parse date." << std::endl;
    }
    
    return 0;
}
```
Sample output:
```
Parsed date: 2023-04-15
```

For more complex formats or when dealing with older C++ versions, third-party libraries like `date.h` (Howard Hinnant's date library) are popular. Here's how you can parse various formats with it:

**Using `date.h` Library:**
Make sure you have the library installed. You can find it [here](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Parsed date: " << parsed_date << std::endl;
    } else {
        std::cout << "Failed to parse date from string." << std::endl;
    }

    return 0;
}
```
Sample output (may vary depending on your system's locale and date settings):
```
Parsed date: 2023-04-15
```
