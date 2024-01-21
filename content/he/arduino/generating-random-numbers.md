---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:48:50.929874-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
ייצור מספרים אקראיים הוא תהליך בו נוצרים מספרים שלא ניתן לחזות מראש את ערכם. תכניתנים משתמשים בכך למטרות שונות: מבחנים, גיוון במשחקים ובחירה נטולת הטיות.

## איך לעשות:
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0)); // הגדלת אקראיות בעזרת קריאה אנלוגית
}

void loop() {
  int randNumber = random(1, 100); // ייצור מספר אקראי בין 1 ל-99
  Serial.println(randNumber);
  delay(1000); // המתנה של שנייה אחת בין כל ייצור
}
```
פלט לדוגמא: 
```
23
67
89
...
```

## ניתוח מעמיק
בעבר ייצור מספרים אקראיים במחשבים היה אתגר גדול מאחר ומחשבים הם דטרמיניסטיים על פי טבעם. אך בעזרת פונקציות כמו `randomSeed()` וקריאות אנלוגיות, אנו יכולים להגדיל את האקראיות.

אלטרנטיבות ליצירת מספרים אקראיים כוללות שימוש בחיישנים חיצוניים כדי לקבל נתונים שאינם צפויים, או באלגוריתמים מתמטיים מורכבים. 

על פרטי המימוש, `randomSeed()` מקבלת ערך כלשהו (seed) שמשמשת נקודת התחלה לאלגוריתם הגנרטיבי - כלומר, עם אותו seed תמיד ייוצרו המספרים האקראיים באותו סדר. לכן, חשוב להשתמש בערך חיצוני משתנה, כמו קריאה אנלוגית שאינה יציבה, כדי להבטיח יותר אקראיות.

## ראו גם
- [תיעוד Arduino לפונקציית random](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [תיעוד Arduino לפונקציית randomSeed](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [מאמר על אלגוריתמים של מספרים אקראיים](https://en.wikipedia.org/wiki/Random_number_generation)