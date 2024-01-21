---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:45:03.101783-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחרוזות חלקיות הן פשוט קטעים מתוך מחרוזת ארוכה יותר. תוכניתנים מחלצים חלקים כאלה כדי לעבד מידע ספציפי, לנתח נתונים, או לשפר את התקשורת עם חיישנים ומכשירים חיצוניים.

## איך לעשות:
```Arduino
String fullString = "Hello, Arduino World!";
String substring = fullString.substring(7, 14); // חלץ את 'Arduino'

void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println(substring); // הצג את המחרוזת החלקית
  delay(2000); // המתן 2 שניות עד להדפסה הבאה
}
```

תוצאת הקוד:
```
Arduino
```

## צלילה עמוקה:
הפונקציה `substring` התחילה את דרכה בשפות תכנות קלאסיות כמו JAVA, ומאז נפוצה לשפות רבות אחרות, כולל C++ של ארדואינו. קיימות אלטרנטיבות כמו פעולות על מערכי תווים, אולם הפונקציה `substring` נותנת פתרון נוח וקריא. בביצוע, הפונקציה יוצרת עותק של הנתונים הרלוונטיים, מה שיכול להשתמש בזיכרון נוסף - נקודה למחשבה בפרויקטים עם משאבים מוגבלים.

## ראה גם:
- [מדריך ארדואינו למחרוזות](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [מאמר על ניהול זיכרון בארדואינו](https://www.arduino.cc/en/Tutorial/Foundations/Memory)