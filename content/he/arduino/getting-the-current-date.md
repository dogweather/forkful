---
title:                "Arduino: קבלת התאריך הנוכחי"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## על מה

ישנם מצבים רבים שבהם חשוב לקבל את התאריך הנוכחי בתכלית במחשבך או בפרויקט הארדווינו שלך. למשל, אם אתה מפעיל חיישנים או שעונים, תוכל להשתמש בתאריך כדי לצפות במידע מדויק יותר בקשר לזמן שנאסף.

## איך לעשות את זה

מקבל תאריך נוכחי בארדוינו הוא מצוין פשוט וקל לביצוע. הנה קוד קצר שיעזור לך להתחיל:

```Arduino
#include <Time.h>

void setup() {
  Serial.begin(9600); // פתיחת קוויר טורד מהשרת לעמוד
  setTime(14, 00, 00, 1, 1, 2020); // קביעת התאריך הנוכחי
}

void loop() {
  digitalClockDisplay();
}

void digitalClockDisplay() {
  // הפקודה הזו תציג את התאריך הנוכחי בפורמט דיגיטלי בקוויר הטורד
  Serial.print(hour());
  printDigits(minute());
  printDigits(second());
  Serial.print(" ");
  Serial.print(day());
  Serial.print(" ");
  Serial.print(month());
  Serial.print(" ");
  Serial.print(year());
  Serial.println(); 
}

void printDigits(int digits) {
  // פקודה להדפסת מספר דו ספרתי לקוויר טורד
  Serial.print(":");
  if(digits < 10)
    Serial.print('0');
  Serial.print(digits);
}
```

כאשר אתה מריץ את הקוד הזה, התאריך הנוכחי יוצג בפורמט דיגיטלי בלוח הטורד.

## מעמקים

כדי לקבל תאריך נוכחי בארדוינו, אתה צריך להתחבר למודול RTC (Real Time Clock). ישנם מודולים שונים זמינים בשוק שיעזרו לך להתחבר למודול RTC שלך בקלות. דבר נוסף שחשוב לשים לב אליו הוא הגדרת התאריך עם הצגה של השעון שלך. זה יכול להיות מעט מורכב כיוון שהגדרת השעון כרוכה בשימוש באטריביור חיצוני.

## ראו גם

- [Tutorial on using the DS3231 RTC module with Arduino](https://howtomechatronics.com/tutorials/arduino/arduino-ds3231-real-time-clock-tutorial/)
- [Arduino Time Library Documentation](https://www.arduino.cc/en/Reference/Time)
- [Real Time Clock Modules on Amazon](https://www.amazon.com/s?k=