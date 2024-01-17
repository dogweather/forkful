---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Kotlin: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

מה ולמה:
המרת מחרוזת לאותיות קטנות היא תהליך בו נמיר מחרוזת משתנה לאותיות קטנות בלבד. תהליך זה נעשה בדרך כלל על מנת להקל על הפעולות השונות בין המחרוזת המקורית למחרוזת המומרת לאותיות קטנות.

איך ל:
```Kotlin
val string = "HELLO WORLD"
val lowerCaseString = string.toLowerCase()
println(lowerCaseString)
```
Output: "hello world"

Deeper Dive:
ההתחלת של העבודה עם מחרוזות ממוקדת על תחילתן של מחרוזות עם אותיות גדולות. המעקב שנעשה לעקב אחר התפתחות התכנים לאורך השנים הכולל הנקבעות המהלכים במחרוזת המקורית. אלטרנטיבות לתהליך המועדף בתכנות כוללות את השימוש בפונקציית מתאימות הפונקציות התיארחו במקרים שיש לנו בידי וערכות משתנים משוכללות לשימוש.

ראה גם:
למידע נוסף על מרת מחרוזת לאותיות קטנות ניתן למצוא במקורות הקשורים הבאים:
- [המדריך הרשמי של Kotlin על מיתאות מחרוזות](https://kotlinlang.org/docs/reference/whatsnew13.html#string)
- [הבלוג הרשמי של Kotlin - כתבה על כיצד לעשות המרת מחרוזת לאותיות קטנות](https://blog.jetbrains.com/kotlin/2013/01/kotlin-m3-1-is-out/#string)
- [ויקיפדיה - מידע על מחרוזת ואפשרויות הקישורים שלה](https://en.wikipedia.org/wiki/String_(computer_science))