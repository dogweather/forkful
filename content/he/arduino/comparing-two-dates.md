---
title:                "Arduino: השוואת שני תאריכים"
simple_title:         "השוואת שני תאריכים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה?

השווה שתי תאריכים ניתן להיות מאתגר ועיקש, אך יכול להיות מאוד שימושי מבחינות תכנותיות ותפעוליות של חומרה כמו Arduino. השוואת תאריכים מאפשרת לנו לבדוק אם תאריך ספציפי כבר עבר, להשוות את שני תאריכים לצורך יצירת אינטרוול בין התאריכים או לבדוק את תקינות התאריך שהמשתמש מזין.

## איך לעשות זאת?

כדי לבצע השוואת תאריכים באמצעות Arduino, נצטרך להשתמש בספריה עבור זמניי ריאל-טיים מתאימים (Real-time clocks) ותאריכים. נמתין לעבור מחזור של זמנים המתאים לבצע אינטרפייס עם התאריכים ונכניס את התאריכים שלנו לבוהן.

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  Serial.begin(9600);
  Wire.begin();
  rtc.begin();
}

void loop () {
  DateTime now = rtc.now();
  Serial.println(now.year());
  Serial.println(now.month());
  Serial.println(now.day());
  Serial.println(now.hour());
  Serial.println(now.minute());
  Serial.println(now.second());
}
```

כאשר נפעיל את הקוד הזה, נוכל לקבל את התאריכים הנוכחיים מתוך התאריך של המחשב. כדי להשוות תאריכים, נשתמש בפונקציות המוגדרות בספריה עבור תאריכים.

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  Serial.begin(9600);
  Wire.begin();
  rtc.begin();
}

void loop () {
  DateTime now = rtc.now();
  DateTime compDate(now.year(), 12, 31); //הגדרת תאריך מפותח למשתנה compDate
  if (now.day() == compDate.day() && now.month() == compDate.month()) {
   //בדיקה האם תאריך היום זהה לתאריך המוגדר בcompDate
    Serial.println("Happy New Year!"); 
  }
}
```

בקוד הזה, אנו משווים את יום וחודש הנוכחיים של התאריך עם יום וחודש הנוכחיים של התאריך המוגדר במשתנה "compDate". אם הם זהים, נדפיס כך במסך "Happy New Year!". את תאריך השנה העברתי ב