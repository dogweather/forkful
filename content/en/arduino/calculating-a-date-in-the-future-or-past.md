---
title:                "Calculating a date in the future or past"
date:                  2024-01-20T17:28:32.675996-07:00
model:                 gpt-4-1106-preview
html_title:           "C# recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"

category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculating a date in the future or past involves determining a specific day before or after a given date. Programmers do this for functions like scheduling events, reminders, or calculating deadlines.

## How to:

Arduino doesn't have built-in date and time functions, but you can use the "TimeLib.h" library to handle date calculations. Make sure you've installed the library before using the below examples.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 0, 0, 25, 3, 2023); // Set time to March 25, 2023, 10:00:00
}

void loop() {
  // Calculate 10 days in the future
  time_t futureTime = now() + 10 * SECS_PER_DAY;
  
  // Print future date
  Serial.print(day(futureTime));
  Serial.print("/");
  Serial.print(month(futureTime));
  Serial.print("/");
  Serial.println(year(futureTime));

  // Calculate 10 days in the past
  time_t pastTime = now() - 10 * SECS_PER_DAY;
  
  // Print past date
  Serial.print(day(pastTime));
  Serial.print("/");
  Serial.print(month(pastTime));
  Serial.print("/");
  Serial.println(year(pastTime));

  // Avoid constant printing
  delay(10000);
}
```
Sample Output:
```
4/4/2023
15/3/2023
```

## Deep Dive

Before RTC (real-time clock) modules and libraries like TimeLib, timekeeping on Arduino was rudimentary and usually manually implemented. There are various ways to compute future or past dates, but using a specialized library like TimeLib simplifies the process significantly.

Alternatives to TimeLib include the more comprehensive "RTClib.h" for use with hardware RTCs, or the built-in `millis()` function for shorter time intervals (with manual date management). TimeLib handles leap years and time zones and provides utility functions for easy date manipulation.

When calculating future or past dates, watch out for time zones and daylight saving changes if you're working with real-time clocks or external time sources. On Arduino, without an RTC or an Internet connection, you'll typically set the time manually or via an external signal (like GPS or radio time signals).

## See Also

- Time Library Documentation:
  https://www.arduino.cc/reference/en/libraries/time/
- RTClib, a popular library for working with real-time clocks:
  https://github.com/adafruit/RTClib
- Arduino's millis() function and its uses:
  https://www.arduino.cc/reference/en/language/functions/time/millis/
