---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Conversion of dates into strings in C++ is the process of turning date objects into readable text format. Programmers do this for easier data handling, display, and making data human-readable for UX/UI.

## How to:

C++ provides a well-rounded library called "chrono" for date and time manipulations, and an additional library "fmt" used for formatting.

Let's jump straight in to see how to convert a system's current date to a string:

```C++
#include <chrono>
#include <fmt/chrono.h>

int main() {
    auto now = std::chrono::system_clock::now();
    std::cout << fmt::format("{:%Y-%m-%d}", fmt::localtime(std::chrono::system_clock::to_time_t(now))) << std::endl;
    return 0;
}
```

This snippet of code will result in the output similar to: `2022-09-23`.

## Deep Dive

Historically, programmers used the C style time library which presented challenges with written locales and non-uniformity of date formats. C++'s modern chrono and format libraries have predominantly replaced these older tools, giving a uniform and type-safe way of handling dates.

C++ offers alternatives, like use of "strftime" from ctime, but the "chrono" and "fmt" ensure consistency and robustness. For instance, the strftime function is locale-dependent which can produce different results based on runtime environment.

Internally, date objects represent a point in time, and converting them to a string involves obtaining year, month, and day and merging them into a formatted string.

## See Also

For further reading and exploring other time related manipulations, check the following links:

- [C++ Chrono library](https://en.cppreference.com/w/cpp/chrono)
- [C++ format library](https://fmt.dev/latest/index.html)
- [C date and time functions](https://en.cppreference.com/w/cpp/chrono/c)

Remember, date and time manipulations are fundamental parts of programming that involve a variety of different operations. Take your time to navigate how best to utilise these facilities depending on your need.