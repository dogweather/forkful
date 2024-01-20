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

Calculating a date in the past or future involves determining the exact date that falls a certain number of days, weeks, months, or years before or after a given date. Programmers do it to perform scheduling, reminders, and time-span calculations.

## How to:

Calculating future or past dates in Arduino is straightforward. We can use the `Date` class of the `Time` library. If the library isn't installed, use Arduino IDE's Library Manager to add it.

Calculate a future date:

```Arduino
#include <TimeLib.h>

void setup() {
 Serial.begin(9600);
 setTime(14, 30, 0, 1, 1, 2022); //set time to 14:30:00 1st Jan 2022

 time_t t = now(); 
 t += (7 * SECS_PER_DAY); //7 days from now

 Serial.println(day(t));
 Serial.println(month(t));
 Serial.println(year(t));
}

void loop() {
 // nothing here
}
```
The output would be `8`, `1`, `2022`, representing 8th January 2022.

## Deep Dive

Historically, date and time calculations were troublesome due to variations in calendars and daylight saving time. The Time library in Arduino has simplified this immensely.

Alternatively, one could use external Real Time Clock (RTC) modules like DS1307 or DS3231, which provide precise, uninterrupted time and date information with functionalities such as alarms.

While the Time library allows for basic date calculations, it doesn't account for leap years in its calculations. So, for precise date manipulations spanning several years, you would need to add leap year correction.

## See Also

For more info on the Time library and its methods, visit the official [Arduino Time library documentation](https://www.arduino.cc/reference/en/libraries/time/). 

To understand working with RTC modules, this [Arduino RTC tutorial](https://howtomechatronics.com/tutorials/arduino/how-to-use-rtc-module-with-arduino/) may be useful.