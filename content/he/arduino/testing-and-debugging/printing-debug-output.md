---
aliases:
- /he/arduino/printing-debug-output/
date: 2024-01-20 17:51:56.392919-07:00
description: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\
  \u05E4\u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D4\u05D9\u05D0 \u05DB\u05DC\
  \u05D9 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DC\u05E8\u05D0\u05D5\u05EA \u05DE\u05D4 \u05E7\u05D5\u05E8\u05D4\
  \ \u05D1\u05D6\u05DE\u05DF \u05D0\u05DE\u05D9\u05EA\u05D9 \u05D1\u05EA\u05D5\u05DA\
  \ \u05D4\u05EA\u05DB\u05E0\u05D9\u05EA. \u05D6\u05D4 \u05E2\u05D5\u05D6\u05E8 \u05DC\
  \u05D0\u05EA\u05E8 \u05D1\u05E2\u05D9\u05D5\u05EA, \u05DC\u05DB\u05D5\u05D5\u05DF\
  \ \u05D5\u05DC\u05D0\u05DE\u05EA \u05E7\u05D8\u05E2\u05D9 \u05E7\u05D5\u05D3."
lastmod: 2024-02-18 23:08:53.117219
model: gpt-4-1106-preview
summary: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD \u05D4\u05D9\u05D0 \u05DB\u05DC\u05D9\
  \ \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DC\u05E8\u05D0\u05D5\u05EA \u05DE\u05D4 \u05E7\u05D5\u05E8\u05D4 \u05D1\
  \u05D6\u05DE\u05DF \u05D0\u05DE\u05D9\u05EA\u05D9 \u05D1\u05EA\u05D5\u05DA \u05D4\
  \u05EA\u05DB\u05E0\u05D9\u05EA. \u05D6\u05D4 \u05E2\u05D5\u05D6\u05E8 \u05DC\u05D0\
  \u05EA\u05E8 \u05D1\u05E2\u05D9\u05D5\u05EA, \u05DC\u05DB\u05D5\u05D5\u05DF \u05D5\
  \u05DC\u05D0\u05DE\u05EA \u05E7\u05D8\u05E2\u05D9 \u05E7\u05D5\u05D3."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלט לניפוי באגים היא כלי שמאפשר למתכנתים לראות מה קורה בזמן אמיתי בתוך התכנית. זה עוזר לאתר בעיות, לכוון ולאמת קטעי קוד.

## כיצד לעשות:
```Arduino
void setup() {
  Serial.begin(9600); // הפעלת התקשורת הסידורית בשיעור באוד של 9600
}

void loop() {
  int sensorValue = analogRead(A0); // קריאת ערך מחיישן
  Serial.println(sensorValue); // הדפסת ערך החיישן
  delay(1000); // המתנה של שנייה
}
```
פלט לדוגמא:
```
512
523
530
```

## לעומק:
בעבר, לפני עידן ה-Arduino, הדפסת פלט לניפוי באגים הייתה מורכבת יותר, דורשת הרכבת ציוד ייעודי ולעיתים אפילו כרטיסי ניפוי. ב-Arduino, היכולת להדפיס לממשק סידורי הופכת את התהליך לפשוט ונגיש יותר. בנוסף, ישנם אלטרנטיבות כמו השימוש ב-LEDs לאיתות על בעיות או עיבוד מקומי של נתונים על גבי ה-Arduino. לאור טבעו, כלי הניפוי הזה מצריך פילוח של זמן פעולה בקוד ויכול להוסיף עומס על התקשורת הסידורית אם לא משתמשים בו באופן חכם.

## ראו גם:
- [התיעוד הרשמי של Arduino לתקשורת סידורית](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [סדנא בנושא ניפוי תוכנות באמצעות הדפסת פלט ב-Arduino](http://playground.arduino.cc/Main/GeneralDebug)
- [פורום Arduino לשאלות ותשובות](https://forum.arduino.cc/)
