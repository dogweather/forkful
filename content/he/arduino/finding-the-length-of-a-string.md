---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

גילוי אורך של שרשרת הוא התהליך שבו אנו מצפים למספר התווים המרכיבים אותה. זה מועיל לצורך אימות קלט שגוי, מניעת עומס על מערכת או למטרות הידוק מקוד.

## איך לעשות:

הצגת השקף של שרשרת ב- Arduino הפשוטה ביותר באמצעות פונקציית `strlen()`. נסו את הדוגמה הבאה:
  
```Arduino
char myString[] = "Hello, World!";

void setup() {
  Serial.begin(9600);
  Serial.println(strlen(myString));
}

void loop() {}
```

פלט דוגמאות: התוכנית מדפיסה 13, האורך של המחרוזת "Hello, World!" (כולל התו , והרווח)

## הצצה לתוך:

הפונקציה `strlen()` נכתבה במקור בשפת C עוד בשנות ה-70, והיא ללא ספק אחת מהתכנית המרובעת ביותר בתכנות מערכות. ישנן דרכים רבות אחרות למצוא אורך שרשרת, כמו למשל לספור את התווים באמצעות צירוף של תנאי ולולאה, אך `strlen()` הפשוטה היא גישה זריזה ויעילה. אמנם, קיים סיכון בהשתמש באמצעות `strlen()` אם התו המנהל (NULL) לא מסתיים במחרוזת, מה שיתכן לגרור לקריסה.

## ראה גם:

[שרשרת בלולאות - דוגמאות בדקדוק C](https://www.learn-c.org/en/Strings_and_loops)  
[נושאי מילול של תווים ושברי שורות](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/char/)  
[מהם משתנים גלובליים?](https://www.arduino.cc/reference/en/language/variables/variable-scope-qualifiers/scope/)

וכך אנו מסיימים שיעור זה. נתראה בפעם הבאה!