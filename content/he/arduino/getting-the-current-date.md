---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
מקבלים את התאריך הנוכחי כדי לדעת מתי נערך פעולה מסוימת. בעזרתו, ניתן לנתח מדדים של היישום שלנו ולשפר את הביצועים.

## איך?
הקוד שלך יהיה דומה לדבר הבא:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup () {
  Serial.begin(57600);

  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  if (! rtc.isrunning()) {
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop () {
  DateTime now = rtc.now();

  Serial.println(now.day());
  Serial.println(now.month());
  Serial.println(now.year());
}
```

הקוד משתמש בספריית RTClib לשליטה על השעון האמיתי. תאריך היום מתעדכן בכל ריצה של הלולאה.

## צלילה מעמיקה
התאריך הנוכחי נעשה באמצעות השעון האמיתי. בעבר, השעונים האמיתיים היו פיסיים ואפשר היה לקבוע אותם ידנית. כיום, עם התפתחות הטכנולוגיה, הם הפכו דיגיטליים יותר ונקלטים אוטומטית. חלופות לספריית RTClib היו יכולות להיות ספריות שעון אמיתי דיגיטלי אחרות או שרתי זמן רשת.

הפרט החשוב ביותר לביצוע הקוד הוא לוודא שהשעון רץ. אם השעון לא מריץ, מתבצעת התאמת תאריך ושעה ע״י 'rtc.adjust'. פונקציה זו מתקבלת את התאריך והשעה מהקומפילר כאשר הקוד מתורגם, כך שהשעון האמיתי יהיה מעודכן.

## ראה גם  
- ספריית ה RTClib: https://adafruit.github.io/RTClib/html/index.html  
- ניתוח זמנים ותאריכים עם ספריית ה-TimeLib: https://www.arduino.cc/reference/en/libraries/timelib/  
- עבודה עם שרתי זמן רשת NTP: https://lastminuteengineers.com/esp8266-ntp-server-date-time-tutorial/