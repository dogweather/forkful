---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:52.227412-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D2\u05D9\u05E9\
  \u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05DC\u05D0 \u05E1\u05E4\u05E8\u05D9\
  \u05D4 \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9."
lastmod: '2024-03-13T22:44:39.782491-06:00'
model: gpt-4-0125-preview
summary: "\u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05DC\u05D0\
  \ \u05E1\u05E4\u05E8\u05D9\u05D4 \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות:
גישה ישירה ללא ספריה צד שלישי:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // דוגמא למחרוזת תאריך בתבנית YYYY-MM-DD
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // אתחול אובייקט DateTime עם הרכיבים שפוענחו
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

פלט דוגמא:
```
Parsed Date: 2023/4/1
```

שימוש בספריה צד שלישי (‏*ArduinoJson* לסצנריות פיענוח מורכבות יותר, כמו קבלת תאריך מתגובת JSON):

ראשית, התקנת ספריית ArduinoJson דרך מנהל ספריות ה-Arduino.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // סימולציה של תגובת JSON
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // חילוץ מחרוזת התאריך
  const char* date = doc["date"];

  // לפרס את התאריך מהמחרוזת כמו בעבר
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

פלט דוגמא:
```
Parsed Date from JSON: 2023/7/19
```
