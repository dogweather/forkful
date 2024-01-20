---
title:                "Getting the current date"
date:                  2024-01-20T15:12:54.604939-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date on an Arduino means querying a real-time clock (RTC) or internet-based time service to find out the date right now. Why do this? Logging events, timestamping data, or scheduling actions—knowing the date can be crucial for these tasks.

## How to:

Let's make our Arduino smart about the date. We'll use an RTC module, like the DS3231, which is precise and has a backup battery.

```arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    // following line sets the RTC to the date & time this sketch was compiled
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  
  delay(3000); // wait for 3 seconds before updating the date
}
```

Sample Output:
```
2023/4/5
```

## Deep Dive:
Historical context? Early computers didn't need to know the date. It wasn't until we got into logging and multi-user systems that it mattered. Nowadays, it's just expected.

Alternatives to RTCs include using the Network Time Protocol (NTP) when connected to the internet, or GPS modules that provide precise time and date information.

Implementation details matter. Not all RTCs are created equal. Some, like the DS1307, are less accurate and can drift more over time. Libraries like `RTClib.h` abstract away the differences between modules, making your life easier.

Using NTP over WiFi requires a different approach. You'd need an ESP8266 or ESP32 with internet access, and to include libraries like `WiFi.h` and `NTPClient.h`. The coding pattern changes— you make periodic requests to a time server and parse the results for the date.

## See Also:
- [RTClib library](https://github.com/adafruit/RTClib): A library that makes interfacing with RTCs a breeze.
- [DS3231 datasheet](https://datasheets.maximintegrated.com/en/ds/DS3231.pdf): The nitty-gritty on the DS3231 RTC module.
- [NTPClient library](https://github.com/arduino-libraries/NTPClient): For getting time over the internet.
- [Time and Date on Arduino Without a RTC](https://create.arduino.cc/projecthub/Arnov_Sharma_makes/time-and-date-on-arduino-without-a-rtc-module-c7d2d6): Alternative methods if you're going RTC-less.