---
title:                "כתיבת בדיקות"
html_title:           "Kotlin: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת טסטים היא תהליך המטרתו לבדוק ולוודא כי קוד התוכנית עובד כצפוי. תהליך זה מקבל קלטים מתוך הקוד ומשווה את הפלט המצופה. כתיבת טסטים הוא חלק בלתי נפרד מתהליך הפיתוח ומסייע באיתור ותיקון באגים בקוד.

## איך לעשות זאת:
קוד כתיבת טסטים בקוטלין הוא פשוט וקצר. ניתן לכתוב סיטואציות שונות ולבדוק את הפלט. לדוגמה, הנה קוד המדגים את הרעיון:

```Kotlin
fun sum(x: Int, y: Int): Int {
    return x + y
}

fun testSum() {
    val sum = sum(2, 3)
    if (sum == 5) {
        println("סיכום: מבצע כמצופה")
    } else {
        println("סיכום: טעות בחישוב")
    }
}
```

פלט הוא: סיכום: מבצע כמצופה

## נעימות מפנקת:
בתחום התכנות, כתיבת טסטים התחילה עם בדיקות ידניות. כיום, ישנם כלים וסיפריות שונות המקלים על פעולת הבדיקה ואף מבטיחים כיסוי קוד מלא. אפשרויות נוספות כוללות שיוכלו לשמש בתחום הטסטים הנם: BDD, TDD וניתוח סטטיסטי של פלט הטסטים.

## ראו גם:
בכדי ללמוד עוד על כתיבת טסטים בקוטלין, ישנם מספר משאבים מומלצים:

- [טטיטוציה: למה כתיבת טסטים היא חשובה](https://tutirial.com/testing-importance)
- [מדריך מהיר ופשוט לכתיבת טסטים בקוטלין](https://tutorial.com/kotlin-testing-guide)
- [מדריך התחלה לכתיבת טסטים עם JUnit ו-Kotlin](https://tutorial.com/junit-kotlin-tutorial)