---
title:                "C recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating a date in the future or past is a common programming task for tasks such as scheduling events, managing deadlines, or displaying information to users based on a specific date. By understanding how to calculate dates in C, you can add functionality to your programs and improve the overall user experience.

## How To

First, let's start by including the necessary libraries:

```C
#include <stdio.h>
#include <time.h>
```

Next, we need to define a variable of type `struct tm` and initialize it with the current date and time using the `time()` function:

```C
struct tm *current_date;
time_t raw_time;
time(&raw_time);
current_date = localtime(&raw_time);
```

Now, we can use the `mktime()` function to convert our current date to a time_t value, which represents the number of seconds since January 1, 1970.

```C
time_t current_timestamp = mktime(current_date);
```

To calculate a date in the future, we can use the `mktime()` function again, but this time adding or subtracting the desired number of seconds from our current timestamp. For example, if we want to calculate the date one week from now, we can add 604800 seconds (60 seconds * 60 minutes * 24 hours * 7 days) to our current timestamp:

```C
time_t future_timestamp = current_timestamp + 604800;
```

We can then use the `localtime()` function again to convert this new timestamp to a readable date:

```C
struct tm *future_date;
future_date = localtime(&future_timestamp);
```

Finally, we can print out the future date in a user-friendly format using the `strftime()` function and the appropriate formatting directives:

```C
char future_date_string[50];
strftime(future_date_string, 50, "%B %d, %Y", future_date);
printf("The date one week from now is: %s\n", future_date_string);
```

This will output the following:

```
The date one week from now is: November 26, 2020
```

To calculate a date in the past, we simply subtract the desired number of seconds from our current timestamp:

```C
time_t past_timestamp = current_timestamp - 604800;
```

And then use the same steps as before to convert it to a readable date and print it out.

## Deep Dive

It's worth noting that the `mktime()` function takes into account leap years and daylight saving time, making it a reliable way to calculate dates. Additionally, the `localtime()` function converts the timestamp to the local time, which is useful for displaying dates to users in their own time zone.

It's also important to keep in mind that the `time_t` type is limited to representing dates between January 1, 1970 and January 19, 2038, which may cause issues for calculating dates far into the future.

## See Also

- [C Standard Library - time.h](https://en.cppreference.com/w/c/chrono)
- [C Date and Time Manipulation](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm)
- [How Does the Unix Timestamp Work?](https://www.epochconverter.com/articles/what-is-a-unix-timestamp)