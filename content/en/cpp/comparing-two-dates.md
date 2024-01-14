---
title:                "C++ recipe: Comparing two dates"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing two dates may seem like a simple task, but it can actually be quite useful in certain programming situations. For example, if you are working on an application that involves scheduling or time-sensitive data, being able to compare dates can help you efficiently organize and filter your data.

## How To

To compare two dates in C++, you can use the `difftime()` function from the `<ctime>` library. This function takes in two `time_t` parameters, which represent the two dates you want to compare. It then calculates the difference between the two dates in seconds and returns it as a `double` value.

Let's take a look at an example code:

```C++
#include <iostream>
#include <ctime>

int main() {

    // Current date
    time_t currTime = time(nullptr);

    // One week ago
    time_t prevTime = currTime - (7 * 24 * 60 * 60);

    // Calculate difference
    double diff = difftime(currTime, prevTime);

    // Output
    std::cout << "Current date: " << ctime(&currTime);
    std::cout << "One week ago: " << ctime(&prevTime);
    std::cout << "Difference in seconds: " << diff << std::endl;

    return 0;
}
```

Output:

```
Current date: Tue Aug 17 22:00:00 2021
One week ago: Tue Aug 10 22:00:00 2021
Difference in seconds: 604800
```

As you can see, the `difftime()` function has calculated the difference between the current date and a week ago in seconds. Keep in mind that the calculation is not limited to just dates within the same month or year. It takes into account the number of days in each month and any leap years between the two dates.

## Deep Dive

Under the hood, the `difftime()` function uses the underlying representation of `time_t` values, which is the number of seconds elapsed since January 1, 1970, at 00:00:00 UTC. This is known as the Unix Epoch time. By subtracting one `time_t` value from another, the function essentially subtracts the number of seconds between the two dates.

It's also worth noting that `difftime()` returns a `double` value, which allows for more precise calculations. This can be useful when working with dates that are very close together, such as milliseconds or microseconds.

## See Also

- [ctime library reference](https://www.cplusplus.com/reference/ctime/)
- [Working with dates and times in C++](https://www.geeksforgeeks.org/working-with-date-and-time-in-cpp/)