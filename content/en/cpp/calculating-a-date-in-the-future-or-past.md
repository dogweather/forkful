---
title:                "Calculating a date in the future or past"
html_title:           "C++ recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past may seem like a mundane task, but it can be extremely useful in many real-world scenarios. It allows us to plan ahead and keep track of important events in our lives.

## How To
```C++
/* First, we need to include the <ctime> header file which contains necessary functions for date and time calculations */
#include <ctime> 

/* Next, we declare a variable of type "time_t" which will hold the current system time */
time_t now = time(0);

/* We can then create a tm structure to store the current date and time values */
struct tm * date = localtime(&now);

/* To calculate a date in the future, we simply add the desired number of days to our current date variable */
date->tm_mday += 7; // Adds 7 days to the current date

/* To calculate a date in the past, we subtract the desired number of days from our current date variable */
date->tm_mday -= 10; // Subtracts 10 days from the current date

/* We can then use the mktime function to convert our updated tm structure back to a time_t variable */
time_t newTime = mktime(date);

/* Finally, we can use strftime to format our new date in a desired manner and store it in a character array */
char* formattedDate = strftime("%m/%d/%Y", localtime(&newTime));

/* We can also print the result to the console for verification */
std::cout << formattedDate << std::endl; // Outputs "MMM/DD/YYYY" format of the calculated date
```

## Deep Dive
Behind the scenes, the <ctime> header file uses the Coordinated Universal Time (UTC) standard to keep track of time. This allows for consistent calculations regardless of the user's local time zone. The tm structure stores all date and time values in integer form, making it easier to manipulate and calculate dates.

Also, the mktime function takes into account any potential changes in daylight saving time, ensuring accurate and reliable date calculations.

## See Also
- [C++ Date and Time Libraries](https://www.geeksforgeeks.org/date-time-class-in-c/)
- [Making Sense of Dates and Times in C++](https://eastmanreference.com/dates-in-c)
- [Using the <ctime> Header File in C++](https://www.programiz.com/cpp-programming/library-function/ctime)