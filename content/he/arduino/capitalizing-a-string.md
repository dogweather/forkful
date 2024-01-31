---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

אותיות רישיות במחרוזת הן כאשר כל אות במילה תחילת באות גדולה. תכניתנים משתמשים בזה להדגשת כותרות, שמות ומשתנים כדי לשפר קריאות ועקביות.

## איך לעשות:

הנה דוגמא פשוטה להמרת מחרוזת לאותיות רישיות בארדואינו:

```Arduino
void setup() {
  Serial.begin(9600);
  String text = "arduino programming";
  text.toUpperCase();
  Serial.println(text);
}

void loop() {
  // לא נדרש לקוד זה
}
```

פלט לדוגמא:
```
ARDUINO PROGRAMMING
```

## עיון מעמיק

העלאת האות הראשונה במילה היא מושג עתיק, מקורו בכתב יד וספרים מודפסים. ב-C, למשל, היית צריך לעבור על כל המחרוזת ולהמיר אות לאות בעזרת הפונקציה `toupper`. בארדואינו, `String.toUpperCase()` הפך את התהליך לפשוט וקומפקטי. חשוב לדעת שהפונקציה משנה את המחרוזת עצמה ולא יוצרת עותק חדש שלה. אם צריך את המחרוזת המקורית, תזדקק לשמור עותק לפני השימוש בפונקציה.

## ראה גם

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [ASCII Table](https://www.asciitable.com/)
