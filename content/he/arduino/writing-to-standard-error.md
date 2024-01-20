---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית (standard error) משמשת לדיווח על תקלות והדפסות לאיתור באגים. מתכנתים משתמשים בזה כדי להפריד בין מידע רגיל לפלט שהוא תוצאת בעיה או לוג איתור באגים.

## איך לעשות:
ב-Arduino, כתיבה לשגיאה סטנדרטית אינה נתמכת באופן ישיר כמו במערכות הפעלה אחרות. במקום זאת, אנחנו משתמשים ב-Serial לדוגמאות פלט לאיתור באגים:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println("הכל תקין.");
  if (detectError()) {
    Serial.println("אופס! יש תקלה."); // שגיאה סטנדרטית אין אבל אפשר לזהות שזו שגיאה על ידי הודעה.
  }
}

bool detectError() {
  // הגדר תנאי שיכול לגרום לתקלה
  return false; // או true אם יש תקלה
}
```

פלט לדוגמה:
```
הכל תקין.
אופס! יש תקלה.
```

## עיון עמוק
במחשבים רגילים יש תהליכים עם תמיכה בפלט סטנדרטי ושגיאות סטנדרטיות. ב-Arduino, יש רק תמיכה בסידורי טוריאלי (Serial communication), שמאפשר שידור מידע למחשב לאיתור באגים או פלט רגיל. אופציה נוספת היא להשתמש בנורות LED לאיתור באגים, כאשר נורה גורפת משמע מתרחשת תקלה.

## ראה גם
- [תיעוד ה-Arduino ל Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Stack Exchange](https://arduino.stackexchange.com/), קהילת שאלות ותשובות עבור תכנות ואיתור באגים ב-Arduino.