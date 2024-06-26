---
date: 2024-01-20 17:36:12.858679-07:00
description: "\u05D0\u05D9\u05DA \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4: \u05D4\u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\
  \u05D9\u05DD \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05EA\u05D7\
  \u05D9\u05DC\u05D4 \u05DB\u05E9\u05D4\u05EA\u05E7\u05E0\u05D9\u05DD \u05E9\u05D5\
  \u05E0\u05D9\u05DD \u05E0\u05D3\u05E8\u05E9\u05D5 \u05DC\u05EA\u05E7\u05E9\u05E8\
  \ \u05E2\u05DD \u05DB\u05DC \u05D0\u05D7\u05D3. \u05D6\u05D5 \u05D3\u05E8\u05DA\
  \ \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D5\u05DE\u05D5\u05D1\u05E0\u05EA \u05DC\u05D0\
  \u05D3\u05DD \u05DC\u05E2\u05D1\u05D3 \u05D5\u05DC\u05EA\u05D0\u05E8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9 \u05EA\u05D0\u05E8\u05D9\u05DA. \u05D4\u05D0\u05DC\u05D8\u05E8\
  \u05E0\u05D8\u05D9\u05D1\u05D5\u05EA\u2026"
lastmod: '2024-04-05T22:50:53.880758-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D4\u05DE\u05E8\u05D4 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\
  \u05D9\u05DD \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05EA\u05D7\
  \u05D9\u05DC\u05D4 \u05DB\u05E9\u05D4\u05EA\u05E7\u05E0\u05D9\u05DD \u05E9\u05D5\
  \u05E0\u05D9\u05DD \u05E0\u05D3\u05E8\u05E9\u05D5 \u05DC\u05EA\u05E7\u05E9\u05E8\
  \ \u05E2\u05DD \u05DB\u05DC \u05D0\u05D7\u05D3."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## איך עושים את זה:
```Arduino
#include <RTClib.h>
#include <Wire.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("לא נמצא RTC");
    while (1);
  }
}

void loop() {
  DateTime now = rtc.now();
  
  char dateStr[20];
  sprintf(dateStr, "%02d/%02d/%04d", now.day(), now.month(), now.year());
  Serial.println(dateStr);

  delay(1000);
}
```
פלט לדוגמא:
```
23/09/2023
```

## נפנוף טכני
ההמרה של תאריכים למחרוזות התחילה כשהתקנים שונים נדרשו לתקשר עם כל אחד. זו דרך פשוטה ומובנת לאדם לעבד ולתאר נתוני תאריך. האלטרנטיבות כוללות את שימוש בספריות נוספות כמו `TimeLib.h`, או המרות בינאריות לאחסון יעיל יותר. לפני כן, פורמט המרה כללי כגון `sprintf` מתאים כאשר אנו רוצים לשלוט בפורמט הסופי של המחרוזת. ובנוסף, זהירות חייבת להינקט כאשר יש צורך לעבוד עם שפות ואזורים שונים כדי להבטיח שפורמט התאריך יהא מובנה למשתמש.

## ראו גם
- [RTClib – ספריית Arduino לעבודה עם RTC](https://github.com/adafruit/RTClib)
- מסמך ה-API של `sprintf`: [cplusplus.com – sprintf](http://www.cplusplus.com/reference/cstdio/sprintf/)
- [Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
