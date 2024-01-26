---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:34:13.526523-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means extracting the date information like day, month, and year, and converting it into a format a computer can understand. Programmers do this because the date and time data is often needed in a structured form to perform operations like comparisons, calculations, or storing in a database.

## How to:

Let's turn a string into a date:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  
  // Let's assume the date string is in the format "DD/MM/YYYY"
  String dateString = "24/12/2023"; 
  
  int day = dateString.substring(0, 2).toInt();
  int month = dateString.substring(3, 5).toInt();
  int year = dateString.substring(6).toInt();
  
  rtc.adjust(DateTime(year, month, day));
  
  Serial.print("Date set to: ");
  Serial.print(day);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(year);
}

void loop() {
  // Doing nothing here
}
```

Sample output:
```
Date set to: 24/12/2023
```

## Deep Dive

Parsing dates has been a common task since the early days of programming. Historically, handling dates was platform-specific and error-prone. The Arduino, with its many libraries like RTClib, simplifies this process considerably.

Alternatives to RTClib for date parsing include using built-in functions or writing custom code to validate and convert date strings. Implementation details such as checking for leap years or dealing with different date formats can make parsing complex. Ensuring input strings are in expected formats and error-checking the parsed values are crucial to avoid glitches.

## See Also

- RTClib on GitHub: https://github.com/adafruit/RTClib
- Arduino Time Library: https://www.arduino.cc/reference/en/libraries/time/
- Arduino DateTime Class reference: https://github.com/adafruit/RTClib/blob/master/DateTime.h
