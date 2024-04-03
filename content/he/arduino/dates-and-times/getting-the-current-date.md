---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:21.015888-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05D4\u05D0\u05E8\u05D3\u05D5\u05D0\u05D9\u05E0\u05D5 \u05E2\u05E6\u05DE\u05D5\
  \ \u05D0\u05D9\u05E0\u05D5 \u05DE\u05DB\u05D9\u05DC \u05E9\u05D9\u05D8\u05D4 \u05DE\
  \u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05E6\u05D5\u05E8\u05DA \u05E7\u05D1\u05DC\
  \u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9\
  \ \u05D1\u05D0\u05D5\u05E4\u05DF \u05D9\u05E9\u05D9\u05E8, \u05DE\u05DB\u05D9\u05D5\
  \u05D5\u05DF \u05E9\u05D0\u05D9\u05DF \u05DC\u05D5 \u05E9\u05E2\u05D5\u05DF \u05D0\
  \u05DE\u05EA (RTC). \u05E2\u05DD \u05D6\u05D0\u05EA, \u05E0\u05D9\u05EA\u05DF \u05DC\
  \u05D4\u05E9\u05D9\u05D2 \u05D6\u05D0\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\
  \u05EA\u2026"
lastmod: '2024-03-13T22:44:39.784270-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D0\u05E8\u05D3\u05D5\u05D0\u05D9\u05E0\u05D5 \u05E2\u05E6\u05DE\
  \u05D5 \u05D0\u05D9\u05E0\u05D5 \u05DE\u05DB\u05D9\u05DC \u05E9\u05D9\u05D8\u05D4\
  \ \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05E6\u05D5\u05E8\u05DA \u05E7\u05D1\
  \u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\
  \u05D9 \u05D1\u05D0\u05D5\u05E4\u05DF \u05D9\u05E9\u05D9\u05E8, \u05DE\u05DB\u05D9\
  \u05D5\u05D5\u05DF \u05E9\u05D0\u05D9\u05DF \u05DC\u05D5 \u05E9\u05E2\u05D5\u05DF\
  \ \u05D0\u05DE\u05EA (RTC)."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

## איך לעשות זאת:
הארדואינו עצמו אינו מכיל שיטה מובנית לצורך קבלת התאריך הנוכחי באופן ישיר, מכיוון שאין לו שעון אמת (RTC). עם זאת, ניתן להשיג זאת באמצעות מודולים חיצוניים של RTC כמו ה-DS3231, וספריות כמו `RTClib`, שפותחה על ידי Adafruit, אשר הופכת את הממשק עם מודולים אלו לפשוט.

ראשית, ודאו שספריית ה-`RTClib` מותקנת בסביבת הפיתוח Arduino IDE שלכם. לאחר מכן, חברו את מודול ה-RTC לארדואינו שלכם על פי ההוראות בתיעוד.

הנה דוגמה פשוטה להתחלה:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    // כאשר יש צורך לקבוע את הזמן במכשיר חדש או לאחר אובדן כוח, אפשר לעשות זאת כאן.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("תאריך נוכחי: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // השהייה של 3 שניות כדי להפחית ספאם בממשק הסריאלי
}
```

פלט לדוגמה (בהנחה שה-RTC שלכם כבר הוגדר):

```
תאריך נוכחי: 2023/4/15
```

הקוד הזה מאתחל את מודול ה-RTC ולאחר מכן, בלולאה, משיג ומדפיס את התאריך הנוכחי למוניטור הסריאלי כל 3 שניות. זכרו, אפשר לבטל את ההערה ולשנות את שורת ה-`rtc.adjust(...)` כדי לקבוע תחילה את תאריך והזמן של RTC או לאחר שהוא איבד כוח.
