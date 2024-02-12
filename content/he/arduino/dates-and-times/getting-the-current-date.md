---
title:                "קבלת התאריך הנוכחי"
aliases:
- /he/arduino/getting-the-current-date/
date:                  2024-02-03T19:09:21.015888-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי בפרויקטים של ארדואינו כוללת את השגת מידע בזמן אמת, אשר יכול להיות קריטי לתיעוד, הצמדת זמן או לתזמון משימות. למתכנתים לעיתים קרובות יש צורך ביכולת זו כדי לשפר פונקציונליות, להבטיח רלוונטיות נתונים, ולהקל על פעולות רגישות לזמן בפרויקטים המשובצים וב-IoT שלהם.

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
