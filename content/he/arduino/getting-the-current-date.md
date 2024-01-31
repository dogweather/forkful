---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:13:25.153599-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי בארדואינו היא פעולה שמאפשרת לתוכנית שלך לדעת איזה יום ושעה עכשיו. זה שימושי ללוחות זמנים, תיעוד אירועים, ולשליטה בפעילויות תלויות זמן.

## איך לעשות:
כדי להשיג את התאריך הנוכחי בארדואינו, תצטרך להשתמש במודול RTC (Real-Time Clock). המודול DS3231 הוא דוגמה פופולרית:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    // כאשר השעון מאבד זיכרון או חשמל בפעם הראשונה, יש לקבוע את התאריך והשעה ידנית:
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  Serial.print("Current Date & Time: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(" ");
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
  delay(1000);
}
```

פלט דוגמה:
```
Current Date & Time: 2023/4/3 15:45:30
```

## עיון נוסף:
ה-DS3231 הוא מודול RTC שנותן שעה דיוקית ומחזיק בזמן בזמן שהמערכת מופעלת וגם כאשר היא לא. בעבר, היו טכניקות פחות מדויקות כמו שימוש בזמני פינג (ping) ובדיקת פולסים. אלטרנטיבות נוספות קיימות כמו NTP (Network Time Protocol), אבל הן דורשות חיבור לרשת.

עדכון אוטומטי של השעון יכול להיעשות על ידי חיבור ל-GPS או לאינטרנט, אבל זה יכול להיות מסובך יותר וזקוק לחומרה נוספת. המודול DS3231 מתכתב עם ארדואינו דרך I2C, וקל לתכנת ולהשתמש.

## ראה גם:
- [DS3231 datasheet](https://datasheets.maximintegrated.com/en/ds/DS3231.pdf)
- [RTClib library on GitHub](https://github.com/adafruit/RTClib)
- [איך לסנכרן שעון RTC עם NTP](https://lastminuteengineers.com/esp32-ntp-server-date-time-tutorial/)
