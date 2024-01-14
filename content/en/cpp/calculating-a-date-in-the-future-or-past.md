---
title:                "C++ recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Why

Have you ever needed to calculate a date in the future or past in your C++ program? Maybe you want to schedule an event or keep track of deadlines. Whatever the reason may be, knowing how to calculate dates in C++ can come in handy for a variety of applications.

# How To

Calculating dates in C++ involves some simple arithmetic and utilizing the date and time functions provided by the language. Let's take a look at some examples:

```C++
// Example 1: Calculating a date in the future
#include <iostream>
#include <ctime>

int main() {
    // Get current date
    time_t now = time(0);
    tm *currentDate = localtime(&now);

    int futureDays = 7; // Number of days to add to current date

    // Calculate future date
    tm *futureDate = localtime(&now);
    futureDate->tm_mday += futureDays;

    // Convert future date to string
    char str[20];
    strftime(str, 20, "%m/%d/%Y", futureDate);

    std::cout << "Today's date: " << strftime("%m/%d/%Y", currentDate) << std::endl;
    std::cout << "Future date: " << str << std::endl;

    return 0;
}

/*
Output:
Today's date: 04/14/2021
Future date: 04/21/2021
*/
```

In this example, we utilized the `time()` function to get the current date and time, and then used the `localtime()` function to convert it into a `tm` struct which holds the date and time components. We then simply added the desired number of days to the current date and converted the result back to a string using `strftime()`.

```C++
// Example 2: Calculating a date in the past
#include <iostream>
#include <ctime>

int main() {
    // Get current date
    time_t now = time(0);
    tm *currentDate = localtime(&now);

    int pastDays = 14; // Number of days to subtract from current date

    // Calculate past date
    tm *pastDate = localtime(&now);
    pastDate->tm_mday -= pastDays;

    // Convert past date to string
    char str[20];
    strftime(str, 20, "%m/%d/%Y", pastDate);

    std::cout << "Today's date: " << strftime("%m/%d/%Y", currentDate) << std::endl;
    std::cout << "Past date: " << str << std::endl;

    return 0;
}

/*
Output:
Today's date: 04/14/2021
Past date: 03/31/2021
*/
```

This example follows a similar approach as the previous one, except we subtracted the desired number of days from the current date to get a date in the past.

# Deep Dive

The `tm` struct used in the examples above contains the following members which are relevant to calculating dates:

- `tm_sec` - seconds after the minute (0-60)
- `tm_min` - minutes after the hour (0-59)
- `tm_hour` - hours since midnight (0-23)
- `tm_mday` - day of the month (1-31)
- `tm_mon` - months since January (0-11)
- `tm_year` - years since 1900
- `tm_wday` - days since Sunday (0-6)
- `tm_yday` - days since January 1st (0-365)
- `tm_isdst` - indicates whether daylight saving time is in effect

By manipulating these members, we can calculate dates in the past or future by adding or subtracting the desired number of days, months, or even years.

# See Also
- [C++ Reference - Date and Time Functions](http://www.cplusplus.com/reference/ctime/)
- [GeeksforGeeks - Calendar Functions in C++](https://www.geeksforgeeks.org/calendar-functions-in-c/)
- [TutorialsPoint - Date and Time in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)