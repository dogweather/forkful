---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Kotlin: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ״מה ולמה?“
חישוב תאריך בעתיד או בעבר הוא בסיסית, הפעלת פעולות אריתמטיות על תאריכים. פרוגרמרים עושים את זה לדעת את הזמן שחלף מאז אירוע מסויים או לתכנות לאירועים שיחרופו בעתיד.

## ״איך לעשות:“
הנה דוגמה פשוטה של איך לחשב תאריך בעתיד:
```Kotlin
import java.time.LocalDate
import java.time.Period

fun main() {
    val now = LocalDate.now()
    val nextWeek = now.plus(Period.ofWeeks(1))
    println("Today is: $now")
    println("One week from today is: $nextWeek")
}
```
הפלט של קטע הקוד יהיה משהו כמו:

"Today is: 2022-05-01"
"One week from today is: 2022-05-08"

## ״עומק להבנה:“
(1) במהלך ההיסטוריה, פרוגרמרים חישבו תאריכים בעבר ובעתיד באמצעות כלים קלאסיים של אריתמטיקה. היום, ספריות שפות תכנות מספקות כלים קלים לשימוש.
(2) חלופות ל-Kotlin כוללות Java, Python, JavaScript, שהן שפות אחרות שיש להם פונקציונליות נרחבת לטיפול בתאריכים וזמנים.
(3) המחלקה LocalDate בספריית Kotlin נותנת דרך פשוטה להתמודד עם תאריכים ללא שיקול השעה ביום.

## ״ראה גם:“
\n
1 [Java's Date and Time Classes](https://docs.oracle.com/javase/tutorial/datetime/index.html)
2 [Python's Datetime Module](https://docs.python.org/3/library/datetime.html)
3 [JavaScript Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)