---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:31:26.213511-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פעולה שמאפשרת לדעת איזה תאריך יהיה או היה לאחר או לפני תקופה מסוימת. תכניתנים מבצעים זאת למטרות כגון תזמון אירועים, תזכורות או חישובי זמן.

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
