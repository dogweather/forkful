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

## What & Why?

Calculating dates in the future or past is the process of determining a specific date based on a given time interval, such as adding or subtracting a certain number of days, weeks, or months from a current date. This is a common task for programmers when working with dates and time in their projects, as it allows for more dynamic and flexible date handling.

## How to:

Calculating dates in the future or past can be done using the `millis()` function in Arduino, which returns the number of milliseconds since the board began running the current program. First, we need to define the desired time interval in milliseconds, then we can add or subtract it from the current time using the `millis()` function. 

```Arduino
unsigned long interval = 86400000; //one day in milliseconds
unsigned long futureDate = millis() + interval; //calculates the future date by adding the interval to the current time
unsigned long pastDate = millis() - interval; //calculates the past date by subtracting the interval from the current time
```

To display the calculated dates, we can use the `Serial.print()` function in Arduino. 

```Arduino
Serial.print("Future Date in milliseconds: ");
Serial.println(futureDate); //displays the calculated future date in milliseconds
Serial.print("Past Date in milliseconds: ");
Serial.println(pastDate); //displays the calculated past date in milliseconds
```

The output from the code above will show the future and past dates in milliseconds, which can then be converted to a more readable format using the appropriate conversion formula. 

## Deep Dive:

Calculating dates in the future or past is a fundamental task in programming and has been used for centuries in various fields such as astronomy, navigation, and finance. In Arduino, the `millis()` function is used to measure time intervals and is based on the Unix time system, which counts the number of seconds since January 1st, 1970.

An alternative method for calculating dates in the future or past is by using the `DateTime` library in Arduino. This library allows for more precise date and time calculations and formatting. However, it may not be suitable for every project as it takes up more memory and may slow down the code execution.

Implementation details for calculating dates in the future or past may vary depending on the project and time system used. It is important to understand the underlying concept and to choose the appropriate method for accurate and efficient results.

## See Also:

- Arduino Reference for `millis()`: https://www.arduino.cc/reference/en/language/functions/time/millis/
- Arduino DateTime Library: https://github.com/PaulStoffregen/Time