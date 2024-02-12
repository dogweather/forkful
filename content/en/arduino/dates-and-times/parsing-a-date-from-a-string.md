---
title:                "Parsing a date from a string"
date:                  2024-02-03T19:02:41.520475-07:00
model:                 gpt-4-0125-preview
simple_title:         "Parsing a date from a string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string in Arduino involves extracting and converting the date components (year, month, day) from a textual representation into a format that can be utilized for timekeeping, comparisons, or manipulations within sketches. Programmers frequently perform this task to interface with components like real-time clocks, loggers, or to process input from web APIs and user interfaces where dates might be presented in a readable format.

## How to:

Direct approach without a third-party library:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Example date string in YYYY-MM-DD format
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Initialize a DateTime object with parsed components
  DateTime parsedDate(year, month, day);
  
  Serial.print("Parsed Date: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Sample Output:
```
Parsed Date: 2023/4/1
```

Using a third-party library (*ArduinoJson* for more complex parsing scenarios, such as obtaining a date from a JSON response):

First, install the ArduinoJson library through the Arduino Library Manager.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simulating a JSON response
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Extracting the date string
  const char* date = doc["date"];

  // Parse the date from the string as before
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Parsed Date from JSON: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Sample Output:
```
Parsed Date from JSON: 2023/7/19
```
