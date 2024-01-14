---
title:    "Arduino recipe: Calculating a date in the future or past"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past may not seem like a useful skill at first, but it can actually come in handy for various projects. For example, if you are creating a project that involves scheduling events or tasks, being able to calculate dates can make your program more efficient and user-friendly. It can also be a fun challenge for those interested in programming and tinkering with Arduino.

## How To

Calculating dates in the future or past can be done using the built-in Arduino functions for date and time. First, you will need to set the current date and time on your Arduino board. This can be done by using the `rtc.adjust(DateTime)` function and specifying the date and time in the format `DateTime(year, month, day, hour, minute, second)`.

Next, you will need to specify the desired date you want to calculate. This can be done by creating a new DateTime variable and setting it to the desired date, also using the `DateTime()` function.

To calculate the difference between the two dates, you can use the `DateTime::diff()` function. This will return a TimeSpan object that contains information about the difference in time between the two dates. From there, you can extract the number of days, months, years, or any other units that you need.

Here's an example of code for calculating the number of days between two dates:

```
Arduino.DateTime start(2021, 3, 15); //set the start date
Arduino.DateTime end(2021, 3, 31); //set the end date
Arduino.TimeSpan diff = end - start; //calculate the difference
int days = diff.totDays(); //extract the number of days
Serial.println(days); //print the result
```

This code will output "16", indicating that there are 16 days between March 15th and March 31st.

## Deep Dive

Aside from calculating the difference between two dates, there are also other functions and methods that can be used for more advanced date and time calculations. For example, the `DateTime.weekday()` function can be used to determine the day of the week for a specific date. This can come in handy if you want to schedule tasks for specific days of the week.

Additionally, there are functions for converting between different time zones, adding or subtracting time from a specific date, and even formatting the date in different styles. By familiarizing yourself with these functions, you can create even more complex and useful date and time calculations for your projects.

## See Also

Here are some helpful resources for further learning about date and time calculations in Arduino programming:

- Arduino DateTime Library: https://playground.arduino.cc/Code/DateTime/
- DateTime functions and methods: https://www.arduino.cc/reference/en/libraries/datetime/
- Time and Date Manipulation with Arduino: https://www.instructables.com/Time-and-Date-Manipulation-With-Arduino/