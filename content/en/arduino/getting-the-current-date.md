---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in Arduino means retrieving the present day, month, and year. We do this for time-tracking, event logging, and managing time-sensitive processes.

## How To:

To get the current date with an Arduino board, you need a Real Time Clock (RTC) module, like the DS1307. Here's a way to do it:

```Arduino
#include <Wire.h>
#include "RTClib.h"
RTC_DS1307 rtc;

void setup () {
  Serial.begin(9600);
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
}

void loop () {
  DateTime now = rtc.now();
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
}
```

Output with sample date:

```Arduino
2022/12/25
```

## Deep Dive

Historically, embedded systems typically barely kept track of time, let alone date, due to their tight hardware constraints. As technology advanced, RTC modules were built to alleviate this issue.

One alternative to the DS1307 RTC would be the DS3231 RTC. The DS3231 has built-in temperature-compensated crystal oscillator (TCXO) and crystal, making it more accurate than the DS1307.

In terms of implementation, the `now()` function of the RTC library fetches the current date. Importantly: your RTC must be correctly set before the `now()` function can return the right date, either via the Arduino or manually.

## See Also:
 
For more information, you might find these links useful:
 
1. Official Arduino Website: https://www.arduino.cc/
2. RTC Library: https://github.com/adafruit/RTClib
3. More on DS1307 RTC: https://circuitdigest.com/microcontroller-projects/arduino-based-real-time-clock.
4. More on DS3231 RTC: https://lastminuteengineers.com/ds3231-rtc-arduino-tutorial/