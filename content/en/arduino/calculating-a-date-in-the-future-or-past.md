---
title:                "Calculating a date in the future or past"
html_title:           "Arduino recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past may be useful in applications such as scheduling and countdown timers. With the Arduino, you can easily incorporate date calculations into your projects.

## How To

First, we need to initialize the Time.h library, which provides functions for working with time and date on the Arduino. Then, we need to set the current date and time using the `setTime()` function. Here's an example of setting the date to May 5, 2020 at 3:00 PM:

```Arduino
#include <Time.h>

void setup() {
  // initialize the Time library
  setTime(15, 0, 0, 5, 5, 2020);
}
```

To calculate a future date, we can use the `now()` function to get the current date and time, and then use simple arithmetic to add the desired time interval. For example, to add 2 days to the current date, we can use the `now()` function and add 2 days in seconds:

```Arduino
#include <Time.h>

void setup() {
  // initialize the Time library
  setTime(15, 0, 0, 5, 5, 2020);
  
  time_t now = now(); // get current date and time
  time_t futureDate = now + (2 * 24 * 60 * 60); // add 2 days in seconds
}
```

Similarly, to calculate a past date, we can subtract the desired time interval from the current date. For example, to subtract 3 months from the current date, we can use the `now()` function and subtract 3 months in seconds:

By utilizing the `weekday()` function, we can also determine the day of the week for a given date. This can be helpful in scheduling events on specific days. Here's an example of using the `weekday()` function to determine if a date falls on a Sunday:

```Arduino
#include <Time.h>

void setup() {
  // initialize the Time library
  setTime(15, 0, 0, 5, 5, 2020);
  
  int day = weekday(2020, 5, 5); // get day of the week for May 5, 2020 (Tuesday)
  
  if (day == 1) { // 1 represents Sunday in the weekday function
    // do something on a Sunday
  }
}
```

## Deep Dive

The Arduino uses a Unix-based time system, where time is represented in the number of seconds that have elapsed since January 1, 1970. This allows for easy calculations using simple arithmetic.

It's important to note that the Arduino has a limited range for time and date calculations, and can only handle values up to January 19, 2038. After that, the time and date will reset to January 1, 1970. If you require calculations beyond this range, you may need to use a different time library.

## See Also

- [Time library reference](https://www.arduino.cc/en/Reference/Time)
- [Generating Random Numbers in Arduino](https://github.com/pi-top/pi-topOS-Web-Portal/wiki/Generating-Random-Numbers-in-Arduino)
- [Using Switch Case in Arduino](https://create.arduino.cc/projecthub/techmirtz/using-switch-case-in-arduino-8c8342)