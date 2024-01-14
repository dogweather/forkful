---
title:    "C++ recipe: Comparing two dates"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why Comparing Two Dates is Useful

As a programmer, you may wonder why comparing two dates is a necessary skill. The answer is simple: in many programming tasks, you may need to compare dates to determine the chronological order of events or to check if a certain date falls within a specific range. Learning how to compare dates in C++ will not only come in handy for everyday programming tasks but also improve your overall coding skills.

## How To Compare Dates in C++

To compare two dates in C++, we use the `std::chrono` library which provides various data types for representing time points and durations. The code below shows a simple example of comparing two dates using this library:

```C++
#include <iostream>
#include <chrono>

int main() {

    // Create two date objects
    std::chrono::system_clock::time_point date1 = std::chrono::system_clock::now();
    std::chrono::system_clock::time_point date2 = std::chrono::system_clock::now() + std::chrono::hours(24);

    // Compare the two dates
    if (date1 < date2) {
        std::cout << "Date 1 is before Date 2." << std::endl;
    } else if (date1 == date2) {
        std::cout << "Date 1 is equal to Date 2." << std::endl;
    } else {
        std::cout << "Date 1 is after Date 2." << std::endl;
    }

    return 0;
}
```

**Sample Output:**
```
Date 1 is before Date 2.
```

As seen in the code, we use the `<` and `==` operators to compare the two dates. The `<` operator checks if the first date is chronologically before the second date, while the `==` operator checks if the two dates are exactly the same. We can also use other comparison operators like `>` and `!=` to compare dates in C++.

## Deep Dive into Comparing Dates in C++

One important thing to note when comparing dates in C++ is that the `std::chrono` library does not support direct comparison between `time_point` and `string` data types. Therefore, we need to first convert the dates to a common data type, such as `int` or `double`, before comparing them. We can do this by using the `std::chrono::duration` function, which calculates the elapsed time between two time points.

Another important aspect to consider when comparing dates is the time zone and daylight saving time. These factors can affect the accuracy of the comparison, so it is essential to take them into account when working with dates.

## See Also

- [Comparing dates in C++](https://www.geeksforgeeks.org/compare-two-dates-c/)
- [std::chrono library reference](https://en.cppreference.com/w/cpp/chrono)
- [Time and date in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)