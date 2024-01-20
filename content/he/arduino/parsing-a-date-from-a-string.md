---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:34:35.493227-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
Parsing a date from a string הוא תהליך שבו ממירים טקסט לאובייקט תאריך. תכנתים זאת כדי לאפשר עיבוד נתונים והשוואות זמן בצורה אוטומטית.

## איך לעשות:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
}

void loop() {
  DateTime now = rtc.now();

  char dateStr[11];
  sprintf(dateStr, "%d/%02d/%02d", now.year(), now.month(), now.day());
  Serial.println(dateStr);

  delay(1000);
}
```
תוצאת דוגמה: `2023/04/01`

## נבחנת העמקה
במקור, עיבוד תאריכים התבצע ידנית ודרש ידע בפורמטים. כיום, ספריות כמו `RTClib` מקלות על התהליך. גישות אלטרנטיביות כוללות שימוש ב-firmware כמו `Time.h` או מערכות זמן מובנות של מיקרו-מעבד. פרטי היישום כרוכים בלימוד ה-API של הספרייה והתאמת הפורמט לצרכים הספציפיים של המיזם.

## ראה גם
- [RTClib GitHub Repository](https://github.com/adafruit/RTClib)
- [Arduino Official Documentation on Date and Time](https://www.arduino.cc/reference/en/libraries/rtclib/#dateTime)