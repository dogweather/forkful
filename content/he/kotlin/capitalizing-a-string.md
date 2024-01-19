---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "Kotlin: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
בגירוי מחרוזת ("string") אנחנו משנים את האות הראשונה של כל מילה במחרוזת לאות גדולה. זה נעשה לשדרג את הצורה של הטקסט ולשפר את הקריאות שלו.

## הוראות:
נראה איך מתבצעת הפונקציה בקוד. בקוטלין, קיימת פונקציה שנקראת `capitalize`.
```Kotlin
fun main() {
    val str = "שלום עולם"
    println(str.capitalize()) // prints "שלום עולם"
}
```
לשימוש באנגלית או שפות נוספות שבהן שימוש רחב באותיות גדולות:
```Kotlin
fun main() {
    val str = "hello world"
    println(str.capitalize()) // prints "Hello world"
}
```
## עומק:
אפשר לשאול, למה בכלל צריך להשתמש בגירוי מחרוזת? מאז הימים הראשונים של המחשבים, היו צריכים להבחין בין מילים מפורשות ומילים המכילות ראשי תיבות. באמצעות הפיכת האות הראשונה במילה לאות גדולה, המחשב יכול להבחין ביניהן. למשל, בהודעות שגיאה.

ישנן אלטרנטיבות לפונקציה `capitalize`. אם אתה רוצה להפוך את כל האותיות לאותיות גדולות, תשתמש בפונקציה `toUpperCase`.
```Kotlin
fun main() {
    val str = "hello world"
    println(str.toUpperCase()) // prints "HELLO WORLD"
}
```
## ראה גם:
ניתן לקרוא עוד על מחרוזות בקוטלין ב[תיעוד הרשמי](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/). שם תוכלו למצוא מידע על כל פונקציות המחרוזת השונות, לרבות `capitalize` ו `toUpperCase`.