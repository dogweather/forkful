---
title:                "Comparing two dates"
html_title:           "C++ recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is a common task in programming where we want to determine whether one date is earlier, later, or the same as another date. Programmers use this comparison to perform logical operations, make decisions, and sort data based on dates.

## How to:

To compare two dates in C++, we first need to import the ```<chrono>``` library. Then, we can use the ```std::chrono::time_point``` data type to store dates. Here's an example code snippet:

```
// Import necessary library
#include <chrono>

// Define two time points
std::chrono::time_point date1 = std::chrono::system_clock::now();
std::chrono::time_point date2 = std::chrono::system_clock::now();

// Compare the two dates
if (date1 < date2) {
  std::cout << "Date1 is earlier than Date2" << std::endl;
}
else if (date1 > date2) {
  std::cout << "Date1 is later than Date2" << std::endl;
}
else {
  std::cout << "Date1 is the same as Date2" << std::endl;
}
```

The output of this code will depend on when you run it, as it uses the current time for the dates. However, it will follow the logic of comparing two dates and print out the appropriate statement.

## Deep Dive:

Historically, before the standardized implementation in C++, dates were compared by converting them into a numerical format. This method was error-prone and could result in incorrect comparisons. With the introduction of the ```<chrono>``` library in C++, programmers now have a reliable and efficient way to compare dates.

Another alternative to comparing dates is to use a third-party library such as Boost.Date_Time. This library offers additional functionalities for working with dates and times in C++.

When comparing dates, it's essential to understand that the comparison is done based on the clock time and not the calendar date. This means that even if two dates fall on the same day, the one with the later time will be considered later.

## See Also:

- [C++ <chrono> library reference](https://en.cppreference.com/w/cpp/header/chrono)
- [Boost.Date_Time library](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)