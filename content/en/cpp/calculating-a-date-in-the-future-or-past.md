---
title:    "C++ recipe: Calculating a date in the future or past"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past may seem like a basic task, but it can be a helpful skill in various programming projects. Whether you need to schedule events, track deadlines, or perform date-based calculations, understanding how to manipulate dates in your code can be beneficial.

## How To

To calculate a date in the future or past, there are a few steps involved. First, you need to understand how dates are represented in programming languages. In C++, dates are typically stored as a structure with separate fields for the month, day, and year.

Next, you will need to use the available built-in functions or libraries to perform date-related operations. Some commonly used functions include `mktime()` to convert a date structure into a time object, `localtime()` to convert a time object into a local date, and `strftime()` to format a date object into a specific string.

Let's look at a simple example of calculating a date in the future using these functions:

```C++
#include <iostream>
#include <ctime>

int main() {
    //create a date struct for today's date
    struct tm current_date;
    current_date.tm_year = 2021; //year is represented in number of years since 1900
    current_date.tm_mon = 7; //months are indexed from 0 (0 = January, 11 = December)
    current_date.tm_mday = 5; //day of the month

    //convert current date into a time object
    time_t curr_time = mktime(&current_date);

    //add 7 days to the current date
    curr_time += 7*24*60*60; //one day = 24 hours * 60 minutes * 60 seconds

    //convert the new time object into a future date
    struct tm future_date = *localtime(&curr_time);

    //format the future date as "Month Day, Year" (e.g. August 12, 2021)
    char formatted_date[20];
    strftime(formatted_date, 20, "%B %d, %Y", &future_date);

    //output the calculated date in a user-friendly format
    std::cout << "The date 7 days from today is: " << formatted_date << std::endl;

    return 0;
}
```

The output of this code would be: `The date 7 days from today is: August 12, 2021`

## Deep Dive

When calculating dates in the future or past, it's important to understand how time zones and daylight saving time can affect your results. Different regions may have different rules and dates for daylight saving time, so it's crucial to account for these differences in your code.

Additionally, you may encounter issues when dealing with dates that fall on a leap year. In this case, special attention must be given to the number of days in February and how the extra day may impact future or past calculations.

There are also various libraries available for C++ that offer date-related functionalities, such as the Boost Date Time Library and the Howard Hinnant's date library. These libraries provide more comprehensive and efficient functions for date and time calculations, and can help simplify the process of calculating dates in the future or past.

## See Also

- [C++ Date and Time Functions](https://www.programiz.com/cpp-programming/library-function/ctime)
- [Boost Date Time Library](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)
- [Howard Hinnant's Date Library](https://github.com/HowardHinnant/date)