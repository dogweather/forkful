---
title:                "Kotlin: כתיבה אל נתוני השגיאה התקניים"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

כאשר אנו כותבים קוד בכל שפת תכנות, חשוב להשתמש בדרכים לניהול שגיאות והתראות. כתיבה לפלט המקרהי נקרא "כתיבה לשגיאה" והיא חשובה מאוד לאחריות למתכנת כאשר משתמשים בשפת תכנות כמו קוטלין.

## איך לעשות זאת

כתיבת פלט המקרהי בקוטלין נעשית באמצעות הפונקציה `System.err.println()` שמקבלת מחרוזת כארגומנט ומדפיסה אותה לפלט המקרהי של התוכנית.

```Kotlin
fun main() {
    val name = "דניאל"
    System.err.println("שם לא תקין: " + name)
}
```

פלט:

```
שם לא תקין: דניאל
```

ניתן להשתמש גם בפונקציה `error()` מתוך הספרייה `kotlin.io` כדי להציג תוכן נוסף וכדי לשלוט על עיצוב התוכן.

```Kotlin
import kotlin.io.*

fun main() {
    val number = 10
    error("מספר לא חוקי: $number")
}
```

פלט:

```
Exception in thread "main" java.lang.IllegalStateException: מספר לא חוקי: 10
```

## העברה עמוקה

פלט המקרהי הוא אמצעי חשוב לאיתור וניהול שגיאות בקוד. על מנת לשלוט על התוכן שנכתב לפלט המקרהי, ניתן להשתמש במתודות נוספות כמו `printStackTrace()` שתציג מידע מפורט יותר על השגיאה ובאפשרותנו להתאים אישית את ההודעה בעזרת `System.err.format()`.

## ראה גם

- [מדריך לשגיאות והתייחסות אליהן בקוטלין](https://kotlinlang.org/docs/exception.html)
- [שפת תכנות קוטלין - השכמות למתכנתים מתחילים](https://www.samanage.com/he/blog/kotlin-programming/#utm_source=kotlinblog&utm_medium=referral&utm_campaign=bmc)
- [קורס למתכנתי פייתון שילמד אותך את העבודה עם קוטלין](https://pythontips.com/2017/09/02/python-coding-an-in-depth-tutorial-for-programmers-new-to-kotlin/)