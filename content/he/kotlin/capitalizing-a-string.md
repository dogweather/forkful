---
title:                "Kotlin: שיפור מחרוזת באותיות רישיות"
simple_title:         "שיפור מחרוזת באותיות רישיות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

כתבתי את המאמר הזה בגלל שהשתמשתי בשיטת לשנות מחרוזת לגדולות בשפת קוטלין ורציתי לשתף את המידע הבעיה עם קהל נרחב של קוראים. 

## כיצד לעשות

```Kotlin
fun capitalizeString(str: string): string {
    return str.capitalize()
}
```

קוד זה ייבא את הפונקציה המובנית `capitalize()` שתגדיל את המחרוזת שהועברה כמשתנה. לדוגמא, אם תכניסו את המחרוזת "kotlin" תקבלו כתוצאה את המחרוזת "Kotlin".

כמו כן, ניתן גם להתייחס לפונקציה `toUpperCase()` המעלה את כל התווים במחרוזת לאותיות גדולות. 

```Kotlin
fun capitalizeString(str: string): string {
    return str.toUpperCase()
}
```

כתובת זו תחזיר "KOTLIN" כתוצאה.

## חקירה מעמיקה

השיטות המוצגות מתארות שימושים פשוטים של כתיבת מחרוזת לאותיות גדולות. אבל ישנן גם שיטות יותר מתקדמות לכתיבת מחרוזות, כגון `toUpperCase(Locale)` שמשמשת להפוך את המחרוזת לאותיות גדולות בשפה מסוימת.

בנוסף, ישנם פונקציות מתנדבות חיצוניות שנבנו על בסיס השיטות הקיימות, והן משתמשות בתכונות ופתרונות מתקדמים יותר לכתיבת מחרוזות לאותיות גדולות. אז אם אתם מעוניינים לעשות יותר מסתם לשנות את המחרוזת לאותיות גדולות, ישנן אפשרויות מתקדמות מאוד שכדאי לחקור.

## ראו גם

- [Kotlin Documentation on Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin String Extensions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html#extensions-for-strings)
- [Kotlin Fun with Strings](https://devrockstars.com/blog/performing-fun-tasks-with-strings-in-kotlin/)