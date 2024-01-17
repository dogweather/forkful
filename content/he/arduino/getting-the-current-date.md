---
title:                "קבלת התאריך הנוכחי"
html_title:           "Arduino: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# מה ולמה? 
מקבל התאריך הנוכחי היא הפעולה של קבלת התאריך הנוכחי ממחשב או מכשיר אלקטרוני והצגתו. מתכנתים מבצעים אותה על מנת להשתמש בזמן נוכחי עבור יישומים שונים, כגון תזכורות יומניות ושעון חכם.

## איך לבצע: 
באמצעות כרטיס הקעקוע Arduino, ניתן לקבל את התאריך הנוכחי בקלות. ניתן לעשות זאת באמצעות המודול RTC (ריאל טיים מכניסה), שמאפשר לקרוא ולרשום את התאריך הנוכחי וטמפרטורת הסביבה. להלן דוגמה של קוד כיצד לקבל את התאריך הנוכחי:

```arduino
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  
  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(" (");
  Serial.print(daysOfTheWeek[now.dayOfTheWeek()]);
  Serial.print(") ");
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
  
  delay(5000);
}
```

כאשר הקוד יורץ, תוצג הודעה של התאריך הנוכחי בפורמט הבא:

2021/8/16 (Monday) 14:01:25

## חקירה מעמיקה: 
כבר מאז תחילת תקופת המחשבים, התאריכים והשעות היו נחשבים לחלק חשוב בתוכניות ויישומים. בתחילתם של מחשבים אלקטרוניים, הייתה צורך לקבל תאריך נכון על מנת לבצע תהליכי תאי דגימה. בשנים האחרונות, עם עלייתם של שעונים חכמים וכרטיסי קעקוע כמו Arduino, קבלת התאריך הנוכחי הפכה לנוחה ופשוטה יותר.

## ראה גם: 
ראה את הקישורים הבאים למידע נוסף על כיצד לקבל את התאריך הנוכחי בכלי תכנות Arduino: 

- [תיעוד של פונקציות DateTime באתר Arduino](https://www.arduino.cc/en/Reference/DateTime)
- [מדריך למודול RTC באתר Random Nerd Tutorials](https://randomnerdtutorials.com/guide-to-ds3231-real-time-clock-rtc-with-arduino/)
- [מדריך להשתמש בזמן נוכחי עם כרטיס קעקוע באתר Howduino](https://howduino.tumblr.com/post/13538877476/545-kicking-it-all-off-disscussion-main-title)