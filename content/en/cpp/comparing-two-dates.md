---
title:    "C++ recipe: Comparing two dates"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Why
As a programmer, you may often come across situations where you need to compare two dates. It could be for sorting purposes or to check if a certain event has occurred before or after another event. In this blog post, we will discuss why comparing two dates is important and how to do it in C++, followed by a deep dive into the intricacies of date comparison.

##How To
To compare two dates in C++, we will first need to create two objects of the `std::chrono::time_point` class. This class represents a point in time and can hold different units of time, such as seconds, minutes, hours, etc. Let's see an example:

```C++
// Defining two time points
std::chrono::time_point start = std::chrono::steady_clock::now();
std::chrono::time_point end = std::chrono::steady_clock::now();

// Comparing the two time points
if (start > end) {
    std::cout << "Start time is greater than end time." << std::endl;
} else if (end > start) {
    std::cout << "End time is greater than start time." << std::endl;
} else {
    std::cout << "Start time and end time are equal." << std::endl;
}
```

In this example, we have used the `std::chrono::steady_clock` class to get the current time and assign it to our `start` and `end` time points. We then use simple `if` statements to compare the two time points and print the appropriate message. 

Another way to compare dates is by using the `std::chrono::duration` class. This class represents a duration of time and can be used to find the difference between two time points. Let's see an example:

```C++
// Defining two time points
std::chrono::time_point start = std::chrono::steady_clock::now();
std::chrono::time_point end = std::chrono::steady_clock::now();

// Finding the duration between the two time points
auto duration = std::chrono::duration_cast<std::chrono::hours>(end - start).count();

// Printing the duration in hours
std::cout << "The duration between start and end time is: " << duration << " hours." << std::endl;
```

In this example, we have used the `std::chrono::duration_cast` function to find the duration between the two time points in hours. This function takes in the desired time unit as a template parameter and returns the duration in that unit.

##Deep Dive
Comparing two dates may seem like a simple task, but it can get tricky when taking into consideration time zones, daylight saving time, and leap years. It is important to make sure that the time points we are comparing are in the same time zone and that any changes in daylight saving time are accounted for.

When comparing dates, we should also pay attention to the precision of our time points. For example, if we need to compare dates up to the millisecond, then we should use `std::chrono::time_point<std::chrono::milliseconds>` instead of just `std::chrono::time_point`.

##See Also
- [C++ Reference for time_point](https://en.cppreference.com/w/cpp/chrono/time_point)
- [C++ Reference for duration](https://en.cppreference.com/w/cpp/chrono/duration)
- [Comparison operators in C++](https://www.geeksforgeeks.org/relational-operators-in-c-cpp/)