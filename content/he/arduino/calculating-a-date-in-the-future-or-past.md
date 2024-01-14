---
title:                "Arduino: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

אנחנו חיים בעולם מאוד מופעל טכנולוגית כי הטכנולוגיה מאפשרת לנו המחשוב והחלקה המהירים של נתונים. אחת התוכניות המעניינות שאתם יכולים לעשות עם טכנולוגיית Arduino היא חישבון תאריך בעתיד או בעבר. כאשר אתם לומדים להשתמש בטכנולוגיית Arduino יעזור לכם לחשב תאריכים ולהכיר את האלגוריתם שמאחורי החישובים האלה. אנחנו נראה כיצד לעשות זאת בצורת קלה ופשוטה עם Arduino.

## כיצד להשתמש בטכנולוגיית Arduino בכדי לחשב תאריכים בעתיד או בעבר

הקוד הבא מראה דוגמא לחישוב תאריכים בעזרת טכנולוגיית Arduino:

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc;

void setup () {
  Serial.begin(57600);
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (! rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
  }
}

void loop () {
  DateTime now = rtc.now();
  DateTime futureDate = now + TimeSpan(7, 12, 30, 6);

  Serial.print("Future date: ");
  Serial.print(futureDate.day(), DEC);
  Serial.print('/');
  Serial.print(futureDate.month(), DEC);
  Serial.print('/');
  Serial.print(futureDate.year(), DEC);

  Serial.print(" at ");
  Serial.print(futureDate.hour(), DEC);
  Serial.print(':');
  Serial.print(futureDate.minute(), DEC);
  Serial.print(':');
  Serial.print(futureDate.second(), DEC);

  Serial.println();
  delay(1000);
}
```

פלט הקוד נראה כך:

```
Future date: 14/12/2020 at 16:33:45
```

כפי שאתם רואים, הוספנו 7 ימים, 12 שעות, 30 דקות ו-6 שניות לתאריך הנוכחי וכעת יש לנו תאריך חדש בעתיד. זהו דוגמא פשוטה לכיצד ניתן לחשב תאריכים בעזרת טכנולוגיית Arduino.

## כיצד זה עובד - חישוב תאריכים בעזרת טכנולוגיית Arduino

חישוב תאריכים בעזרת טכנולוגיית Arduino משתמש באלגוריתם שנקרא "תאריך יוליאני". תאריך יוליאני הוא שיטת חישוב תאריכים המתבסס