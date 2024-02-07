---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:55:32.029315-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

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
