---
title:                "פרסום תאריך ממחרוזת"
aliases:
- he/arduino/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:52.227412-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

פיענוח תאריך ממחרוזת ב-Arduino כולל חילוץ והמרת רכיבי התאריך (שנה, חודש, יום) מייצוג טקסטואלי לתבנית שניתן להשתמש בה לשמירת זמן, השוואות או מניפולציות בתוך סקיצות. מתכנתים מבצעים לעיתים קרובות משימה זו כדי להתממשק עם רכיבים כמו שעונים בזמן אמת, מקליטים, או לעבד קלט מ-APIs של אינטרנט ומממשקי משתמש שבהם תאריכים עשויים להופיע בתבנית קריאה.

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
