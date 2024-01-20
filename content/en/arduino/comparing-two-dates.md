---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparing Two Dates with Arduino 

## What & Why?
Comparing two dates involves determining if one date is before, after, or equal to another. Programmers do this often to set up timers, schedule events, or record data over a certain period.

## How to:
```Arduino
#include <TimeLib.h>

time_t t1 = now(); // Current time.
delay(5000); // 5-second delay to create a difference.
time_t t2 = now(); // New time.

if(year(t1) != year(t2)){ 
    Serial.println(year(t2) - year(t1)); 
}
else if(month(t1) != month(t2)){ 
    Serial.println(month(t2) - month(t1)); 
}
else if(day(t1) != day(t2)){ 
    Serial.println(day(t2) - day(t1)); 
}
else if(hour(t1) != hour(t2)){ 
    Serial.println(hour(t2) - hour(t1)); 
}
else if(minute(t1) != minute(t2)){ 
    Serial.println(minute(t2) - minute(t1)); 
}
else { 
    Serial.println(second(t2) - second(t1)); 
}
```
This code compares two dates on different levels starting from years down to seconds. It only prints out the difference if it finds one.

## Deep Dive
Historically, comparing dates was more complex, dealing directly with UNIX timestamps or hefty date strings. Arduino's TimeLib simplifies this task with accessible date units. 

While TimeLib is standard, alternatives like RTClib for RTC modules, or built-in date comparison functions in more advanced boards (like ESP32) exist.

Remember, time_t values (UNIX timestamp) are stored as the number of seconds passed since the beginning of 1970. The library’s functions convert this to a more digestible form like year, month, hour, etc.

## See Also
- Time library for Arduino: [https://www.arduino.cc/reference/en/libraries/time/](https://www.arduino.cc/reference/en/libraries/time/)
- Dating with ESP32: [https://randomnerdtutorials.com/esp32-date-time-ntp-client-server-arduino/](https://randomnerdtutorials.com/esp32-date-time-ntp-client-server-arduino/)