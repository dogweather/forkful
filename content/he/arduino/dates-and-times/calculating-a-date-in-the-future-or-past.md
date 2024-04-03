---
date: 2024-01-20 17:31:26.213511-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.789261-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך לעשות:
```Arduino
#include <TimeLib.h> // ספרייה לחישובי זמן

void setup() {
  setTime(10, 30, 0, 1, 1, 2023); // הגדרת זמן תחילתי: 01/01/2023, 10:30:00
  Serial.begin(9600);
  printFutureDate(30); // הדפסת תאריך לאחר 30 ימים
}

void loop() {
  // בדוגמא זו, הלולאה הראשית ריקה.
}

void printFutureDate(int daysInFuture) {
  time_t future = now() + daysInFuture * SECS_PER_DAY; // חישוב הזמן בעתיד
  Serial.print(day(future));    // יום
  Serial.print("/");            // מפריד בין יום לחודש
  Serial.print(month(future));  // חודש
  Serial.print("/");            // מפריד בין חודש לשנה
  Serial.println(year(future)); // שנה
}
```
תוצאה לדוגמא: `31/1/2023`

## צלילה לעומק
לחישוב תאריך בעבר, היו תכנתים בעבר כותבים אלגוריתם מורכב. היום, ספריות כמו `TimeLib` מקלות על המשימה ב-Arduino. יש אלטרנטיבות אחרות, כגון `RTC` (Real-Time Clock) מבוססי חומרה לדיוק גבוה יותר. פרטי המימוש כוללים טיפול בעקביות חישוב הזמן, קפיצות שנה וזיהוי חודשים ושנים מעוברות.

## ראו גם
- דוקומנטציית `TimeLib`: http://www.pjrc.com/teensy/td_libs_Time.html
- דף התיעוד של `RTClib`: https://adafruit.github.io/RTClib/html/index.html
- מדריך ל`DS3231 RTC` Module: https://lastminuteengineers.com/ds3231-rtc-arduino-tutorial/
