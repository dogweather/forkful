---
title:                "Getting the current date"
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date is the process of retrieving and displaying the current date on a device or program. Programmers often do this in order to keep track of time and make accurate timestamps for data. This is especially important in applications such as data logging and event scheduling.

## How to:
```Arduino

// Include necessary library
#include <RTClib.h>

// Create an instance of the RTC_DS3231 class
RTC_DS3231 rtc;

void setup() {
    // Initialize serial monitor for debugging
    Serial.begin(9600);
    while (!Serial);

    // Initialize RTC
    if (!rtc.begin()) {
        Serial.println("Couldn't find RTC");
        while (1);
    }

    // Set the date and time for RTC
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
}

void loop() {
    // Get current date and time from RTC
    DateTime now = rtc.now();

    // Print the current date to serial monitor
    Serial.print(now.day());
    Serial.print('/');
    Serial.print(now.month());
    Serial.print('/');
    Serial.print(now.year());
    Serial.println();

    // Print the current time to serial monitor
    Serial.print(now.hour());
    Serial.print(':');
    Serial.print(now.minute());
    Serial.print(':');
    Serial.print(now.second());
    Serial.println();

    // Wait one second before repeating loop
    delay(1000);
}
```

**Output:**
```
29/01/2022
11:34:57
```

## Deep Dive:
The concept of timekeeping has evolved over the years, from primitive methods such as tracking the sun's movement to modern technology such as atomic clocks. In the world of programming, getting the current date involves using a Real-Time Clock (RTC) module or utilizing an internet connection to access a Network Time Protocol (NTP) server. Both methods have their advantages and disadvantages.

An RTC module is a hardware component that keeps track of time even when the device is powered off. This makes it a reliable option for precise timekeeping. On the other hand, using an NTP server requires an internet connection and may not be accurate if there are network issues. However, it allows for automatic synchronization of time across multiple devices.

In Arduino, the most commonly used library for retrieving the current date is the RTClib library. This library provides an easy-to-use interface for controlling RTC modules. In the example code above, we used the RTC_DS3231 class from this library to set and get the current date and time.

## See Also:
- [Arduino RTClib library reference](https://learn.adafruit.com/adafruit-rtc-libraries/rtc-ds3231)
- [Difference between RTC and NTP](https://www.electronicshub.org/rtc-vs-ntp/)