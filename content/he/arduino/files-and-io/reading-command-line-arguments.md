---
date: 2024-01-20 17:55:32.029315-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D0\u05E1\u05E4\u05E7\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\
  \u05D9\u05DD \u05DC\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05DE\u05D4\u05DE\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05E8\u05D2\u05E2 \u05D4\u05E4\u05E2\u05DC\u05EA\u05D4\
  . \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05D0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD\
  \ \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4 \u05DC\u05E6\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\
  \u05E6\u05D4, \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.792718-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D0\u05E1\u05E4\u05E7\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\
  \u05D9\u05DD \u05DC\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05DE\u05D4\u05DE\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05E8\u05D2\u05E2 \u05D4\u05E4\u05E2\u05DC\u05EA\u05D4\
  . \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05D0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD\
  \ \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4 \u05DC\u05E6\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\
  \u05E6\u05D4, \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא אספקת פרמטרים לתוכנית מהמשתמש ברגע הפעלתה. תוכניתאים עושים זאת כדי להתאים את התוכנה לצרכים משתנים בזמן ריצה, מבלי לשנות את הקוד עצמו.

## איך לעשות:
הנה, פלטפורמת Arduino אינה תומכת באופן טבעי בקריאת פרמטרים משורת פקודה כמו שפתוח מחשב אופייני מסוגל לעשות. ניתן לדמות תכונה זו על ידי קבלת קלט מממשק סדרתי:

```Arduino
void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // המתן לחיבור הממשק הסדרתי
  }
  Serial.println("הזן ארגומנטים:");
}

void loop() {
  if (Serial.available() > 0) {
    String arg = Serial.readStringUntil('\n');
    Serial.print("קיבלתי: ");
    Serial.println(arg);
    // כאן תבצע מהלכים בהתאם לארגומנט
  }
}
```
פלט לדוגמא: כאשר המשתמש מזין "אור" בממשק הסדרתי, ה-Arduino יחזיר "קיבלתי: אור".

## צלילה לעומק:
קריאת ארגומנטים משורת פקודה היא פרקטיקה נפוצה במחשבים אבל לא במיקרו-קונטרולרים כמו Arduino. במחשבים, ניתן להשתמש בארגומנטים אלו לעשות שינויים בהתנהגות התוכנית על פי הקלט מהמשתמש. ב-Arduino, שימוש בממשק סדרתי הוא אלטרנטיבה נפוצה כשברצונך לקלוט קלט חיצוני. פרט לכך, במקרים מסוימים נעשה שימוש בכרטיסי SD או בזיכרון EEPROM כדי לאחסן ולקרוא ערכים שישמשו כפרמטרים לפעולות התוכנית.

## ראה גם:
- [מראה כיצד להשתמש בממשק הסדרתי של Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [מדריך לקריאה וכתיבה לכרטיס SD ב-Arduino](https://www.arduino.cc/en/reference/SD)
- [עבודה עם זיכרון EEPROM ב-Arduino](https://www.arduino.cc/en/Reference/EEPROM)
