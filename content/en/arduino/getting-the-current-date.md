---
title:    "Arduino recipe: Getting the current date"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
One of the basic functions of a microcontroller like Arduino is to keep track of time. Knowing the current date may seem like a simple task, but it is actually a crucial element in many projects. From automatically switching on lights on a specific date to creating a timed watering system for your plants, getting the current date is an essential aspect of programming with Arduino.

## How To
To get the current date on an Arduino board, we will use the built-in function `millis()` in combination with the `time_t` datatype from the "Time" library.

```
Arduino #include <Time.h>
unsigned long secondsSinceEpoch = millis() / 1000;
time_t currentDateTime = secondsSinceEpoch;
String currentDate = month(currentDateTime) + "/" + day(currentDateTime) + "/" + year(currentDateTime);
Serial.println(currentDate);
```

The first line includes the "Time" library, which allows us to use functions for time and date. We then use the `millis()` function to get the number of milliseconds that have passed since the Arduino board was last started. We divide this number by 1000 to get the number of seconds and store it in an `unsigned long` variable.

Next, we assign this variable to a `time_t` datatype, which will store the number of seconds since January 1, 1970. This is known as the Unix epoch. Lastly, we use the `month()`, `day()`, and `year()` functions to extract the current month, day, and year from the `currentDateTime` variable. We concatenate these values into a `String` and print it out using `Serial.println()`.

If we upload this code to our Arduino board, we should see the current date in the serial monitor. 

```
10/14/2021
```

## Deep Dive
Now, let's take a closer look at the `time_t` datatype and the "Time" library. The `time_t` datatype is used to store the number of seconds since the Unix epoch, which is a common way of representing time and date in computers. The "Time" library provides functions for converting the raw number of seconds into more readable values such as month, day, year, hour, and minute. It also has functions for setting and adjusting the current time and date on the Arduino board.

In our previous example, we only used the `month()`, `day()`, and `year()` functions, but there are many other useful functions in the "Time" library. For example, the `hour()`, `minute()`, and `second()` functions can be used to get the current time in addition to the date.

## See Also
- [Time library reference](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Unix time](https://en.wikipedia.org/wiki/Unix_time)
- [Github repository for Time library](https://github.com/PaulStoffregen/Time)