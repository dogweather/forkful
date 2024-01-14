---
title:                "Arduino recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to calculate a date in the future or past for a project, but didn't know where to start? Look no further, because in this blog post, we will show you how to use Arduino to easily calculate dates in the future or past. Whether you're working on a personal project or a school assignment, this skill will come in handy.

## How To

First, let's go over the basics of calculating dates in the future or past. We will be using the built-in functions in Arduino, so there's no need to import any external libraries. First, we need to set the current date using the `DateTime` function. For example:
```Arduino
DateTime now (2021, 11, 25);
```
This will set the current date to November 25, 2021. Next, define the duration in the future or past that you want to calculate. For this example, we will use 10 days.
```Arduino
unsigned long duration = 10;
```
Finally, we can use the `addTime` function to calculate the date in the future or past. The syntax for this function is `addTime(currentDate, duration)`. So, for our example, the code would look like:
```Arduino
DateTime newDate = addTime(now, duration);
```
This will give us the date 10 days after November 25, 2021, which is December 5, 2021.

But what if you want to calculate the date based on a specific calendar system, such as the Julian or Gregorian calendar? You can use the `addTimeDifferentCalendar` function, which has the syntax `addTimeDifferentCalendar(currentDate, duration, calendar)`. For example, if we want to calculate the date 10 days after May 17, 2021 in the Julian calendar, the code would be:
```Arduino
DateTime newDate = addTimeDifferentCalendar(DateTime (2021, 5, 17), 10, Julian);
```

## Deep Dive

The `addTime` and `addTimeDifferentCalendar` functions are part of the `DateTime` library in Arduino, which allows you to work with date and time variables. These functions use the built-in `atof` function to convert the duration into a floating-point number, making it easier to calculate dates with decimals (for example, 0.5 days).

Additionally, the `addTimeDifferentCalendar` function takes into account leap years and leap seconds, making it more accurate for different calendar systems.

## See Also

For more information on the `DateTime` library and its functions, check out these resources:

- [Arduino Reference - addTime function](https://www.arduino.cc/reference/en/libraries/datetime/addtime/)
- [DateTime library documentation](https://playground.arduino.cc/Code/DateTime/)
- [Calculating Julian dates in Arduino](https://forum.arduino.cc/t/julian-date-time/497669)