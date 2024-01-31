---
title:                "המרת תאריך למחרוזת"
date:                  2024-01-20T17:36:12.858679-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת היא תהליך שבו לוקחים תאריך ושומרים אותו כטקסט. זה נעשה כדי לקל על התצוגה, השמירה או הלוגיקה בקוד שלנו.

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
