---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מונח "הגדלת אותיות" בהקשר של מחרוזות מתייחס להמרת כל התווים במחרוזת לאותיות גדולות (רישיות). תכניתנים משתמשים בזה לצורכי עקביות, עיבוד טקסט וממשק משתמש.

## איך לעשות:
ב-Kotlin, תהליך ההגדלת אותיות פשוט. נעשה שימוש בפונקציה `.uppercase()`:

```Kotlin
fun main() {
    val original = "shalom"
    val capitalized = original.uppercase()
    println(capitalized) // Output: SHALOM
}
```

קל וברור, נכון?

## להתעמקות

בעבר, בשפות תכנות אחרות ואף בגרסאות ישנות יותר של Kotlin, השימוש היה בפונקציה אחרת בשם `.toUpperCase()`. מאז Kotlin 1.5, `.uppercase()` היא האפשרות המומלצת. 

למרות שהמרה לאותיות גדולות נראית פשוטה, יש מקרים מורכבים, כמו תווים בשפות שונות שהתנהגותם שונה בעת ההמרה. 

אלטרנטיבה אחרת היא `.capitalize()` שמגדילה רק את האות הראשונה של המחרוזת, אך שימו לב שהיא נחשבת ללא מומלצת (deprecated) מ-Kotlin 1.5 ואילך.

## לקרוא גם

- [Kotlin's Standard Library Documentation for String functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- [Kotlin's REPL (Read-Eval-Print Loop) to test code snippets](https://try.kotlinlang.org/)
- [Official Kotlin Coding Conventions](https://kotlinlang.org/docs/coding-conventions.html)

במידה ואתם מעוניינים להתמקצע יותר בנושאים הקשורים לעיבוד מחרוזות והמרות תווים, כדאי לחפש מאמרים ומדריכים שספציפיים לשפה שעובדים איתה, כי כל שפה יש את הדרכים הייחודיות שלה להתמודד עם אתגרים אלה.
