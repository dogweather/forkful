---
title:                "הדפסת פלט לניפוי באגים"
aliases:
- he/arduino/printing-debug-output.md
date:                  2024-01-20T17:51:56.392919-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/printing-debug-output.md"
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
