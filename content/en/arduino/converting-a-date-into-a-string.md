---
title:                "Arduino recipe: Converting a date into a string"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

If you're new to Arduino programming, you may be wondering why someone would want to convert a date into a string. The reality is, there are many applications where you may want to display the date on an LCD screen or store it in a data logger. Converting a date into a string makes it easier to work with and display in a readable format.

## How To

Converting a date into a string in Arduino is a simple process. Here is an example using a DS1307 real-time clock module:

```
Arduino Wire Library for I2C communication
#include <Wire.h>

Arduino RTClib Library for DS1307
#include "RTClib.h"

//Initialize the RTC object
RTC_DS1307 rtc;

void setup() {
  //Initialize serial communication
  Serial.begin(9600);
  
  //Initialize I2C communication
  Wire.begin();

  //Check if the RTC is running, if not, set the date and time
  if (!rtc.begin()) {
    Serial.println("RTC is not running!");
    rtc.adjust(DateTime(__DATE__, __TIME__));
  }

  //Read the current date and time from the RTC
  DateTime now = rtc.now();

  //Convert the date into a string
  String dateString = String(now.day()) + "/" + String(now.month()) + "/" + String(now.year());

  //Convert the time into a string
  String timeString = String(now.hour()) + ":" + String(now.minute()) + ":" + String(now.second());

  //Print the date and time in a readable format
  Serial.println("Current Date: " + dateString);
  Serial.println("Current Time: " + timeString);
}

void loop() {
  //Do nothing
}
```

**Sample Output:**
```
Current Date: 23/8/2021
Current Time: 16:48:54
```

## Deep Dive

The process of converting a date into a string involves using the `DateTime` object from the RTClib library to retrieve the current date and time data from the DS1307 module. Then, the `String()` function is used to convert the individual components (day, month, year, hour, minute, second) into strings and concatenate them together with the appropriate formatting characters (e.g. "/"). Finally, the date and time strings are printed to the serial monitor for testing purposes, but you can use them however you like in your project.

One thing to note is that when using the `DateTime` object, you may need to account for any potential variations in the date and time formats. For example, in some cases, the month or day may only be represented by a single digit. In these cases, you can use conditional statements or padding functions to ensure that the formatting of your date and time strings remains consistent.

## See Also

- [Arduino Wire Library](https://www.arduino.cc/en/reference/wire)
- [RTClib Library for DS1307](https://github.com/adafruit/RTClib)
- [DateTime Object Reference](https://www.pjrc.com/teensy/td_libs_RTC.html)