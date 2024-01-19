---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Getting the Current Date in C++

## What & Why?

Accessing the present date in C++ programming is a common task, often used in programs that deal with time, schedules, or logging. This data can be utilized for timestamping events, handling time-restricted operations, or managing user sessions.

## How to:

Let's get our hands dirty with the code to get the current date in C++. We'll use the `chrono` library, which has been a part of standard C++ since C++11.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Get current time
    auto now = std::chrono::system_clock::now();
    // Convert to time_t for manipulation
    std::time_t now_time_t = std::chrono::system_clock::to_time_t(now);
    
    // Output Current date 
    std::cout << "Current date: " << std::ctime(&now_time_t);

    return 0;
}
```
Upon executing this program, you will get an output similar to the one given below:

```
Current date: Sun Sep 15 21:11:38 2019
```
## Deep Dive

Originally, C++ did not have a dedicated toolset for managing dates and times, thus programmers typically augmented the C programming language's way of managing it via `time.h` or `ctime` libraries. However, with the introduction of C++11, the `chrono` library was introduced, offering a higher-level, more convenient way to handle time and date.

However, it's not your only choice. If you're working with an older codebase or can't or don't pull in `chrono`, the more traditional `time_t` type and the `localtime` function still work well:

```C++
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0);
    std::tm now = *std::localtime(&t);
    
    // Output Current date 
    std::cout << "Current date: " << now.tm_mday << '-' <<  now.tm_mon + 1  << '-' << 1900 + now.tm_year;

    return 0;
}
```
In certain situations, you might prefer external libraries such as `boost::date_time`. This can provide more capabilities, such as parsing string dates, more straightforward timezone handling, and more, at the cost of an extra dependency.

## See Also

- C++ Reference: [`std::chrono` library](https://en.cppreference.com/w/cpp/chrono)
- Microsoft C++ Docs: [`<chrono>`](https://docs.microsoft.com/en-us/cpp/standard-library/chrono?view=msvc-160)
- Boost.org: [`boost::date_time` library](https://www.boost.org/doc/libs/1_54_0/doc/html/date_time.html)