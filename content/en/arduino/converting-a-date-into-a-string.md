---
title:                "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
As an Arduino programmer, you may come across a scenario where you need to display the date on a LCD screen or log data with a timestamp. In such cases, it is necessary to convert the date into a string format so that it can be easily displayed or stored. In this blog post, we will explore how to convert a date into a string using Arduino.

## How To
Converting a date into a string can be accomplished by using the built-in function `strftime()` from the Arduino Time Library. This function takes in a date and time structure and formats it into a string according to the specified format.

```Arduino
#include <TimeLib.h> // Include Time Library
tmElements_t time; // Create a time structure

// Set the date and time using a Unix timestamp
time_t t = 1588657100; // Representing May 05, 2020 10:05:00
breakTime(t, time); // Break time into individual elements

// Convert the date into a string using strftime function
char dateString[20]; // Define a char array for the string
strftime(dateString, sizeof(dateString), "%d/%m/%Y", &time); // Format: DD/MM/YYYY

// Print the date string
Serial.println(dateString); // Output: 05/05/2020
```

In the code above, we first include the Time Library and create a `tmElements_t` structure to store the date and time. Then, using a Unix timestamp representing a specific date and time, we break it down into individual elements using the `breakTime()` function. Finally, we use the `strftime()` function to format the date into a string and print it to the Serial Monitor.

## Deep Dive
The `strftime()` function allows you to specify the format of the output string by using format specifiers. Some commonly used format specifiers for date are:

- `%d` for day of the month (01-31)
- `%m` for month (01-12)
- `%Y` for year (4 digits)
- `%y` for year (2 digits)

There are also options to include the day of the week, time, and other variations. You can find a list of all the format specifiers and their meanings in the Arduino Time Library documentation.

Additionally, the `strftime()` function also allows you to convert the date and time into different time zones by using the `setTime()` and `setTZ()` functions. This can be useful if you want to display the date and time in a specific location or adjust for daylight saving time.

## See Also
- [Arduino Time Library documentation](https://www.arduino.cc/reference/en/libraries/time/)
- [TimeLib.h Library Reference](https://github.com/PaulStoffregen/Time/blob/master/TimeLib.h)