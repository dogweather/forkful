---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא אחת מהפעולות הבסיסיות בתכנות, בה מפכילים את כל האותיות שבמחרוזת לאותיות קטנות. תכנתים מבצעים זאת כדי להקל על השוואת מחרוזות, במיוחד במקרים כשהמשתמש הזין טקסט שגוי או באופן לא עקבי.

## איך:
נושאים מחרוזת ומשתמשים בפונקציה toLowerCase() כדי להמיר את כל האותיות הגדולות שבה לאותיות קטנות. הנה דוגמה:

```Kotlin
val str = "Hello, World!"
val lowerCaseStr = str.toLowerCase()
println(lowerCaseStr) // prints: "hello, world!"
```

## רקע עמוק יותר:
העיקרון של המרת מחרוזת לאותיות קטנות הוא קיים בתכנות כבר מהימים הראשונים שלה. ב-Kotlin, אפשר לעשות זאת גם באמצעות שימוש ב- map ו- Character.toLowerCase(). יש לציין כי toLowerCase() משתמש בהגדרות של מערכת ההפעלה, כך שאם אתם מעוניינים בפלט קבוע, עדיף לשלוט על ה-Locale שלכם.

```Kotlin
val str = "Hello, World!"
val lowerCaseStr = str.map { it.toLowerCase() }.joinToString("")
println(lowerCaseStr) // prints: "hello, world!"
```
## ספריות ומשאבים נוספים:
1. **Official Kotlin Documentation**: אתר התיעוד הרשמי של Kotlin מסביר המרת מחרוזת לאותיות קטנות בצורה מקיפה. ([לינק](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html))
2. **Stack Overflow**: מני דיונים ושאלות שנעשו בנוגע לנושא. ([לינק](https://stackoverflow.com/questions/43409645/if-i-want-to-change-all-character-of-string-to-lower-case-using-kotlin-what-is))