---
title:                "השוואת שתי תאריכים"
aliases:
- /he/kotlin/comparing-two-dates.md
date:                  2024-01-20T17:33:47.038634-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שתי תאריכים זהו פעולה שבה אנו בודקים האם תאריך אחד הוא לפני, אחרי או זהה לתאריך אחר. תכניתנים עושים זאת על מנת למיין נתונים, לבצע בדיקות תקינות, או לחשב פרקי זמן.

## איך לעשות:
```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 3, 14)
    val date2 = LocalDate.of(2023, 5, 18)

    println("Is date1 before date2? ${date1.isBefore(date2)}")  // true
    println("Is date1 after date2? ${date1.isAfter(date2)}")    // false
    println("Is date1 equal to date2? ${date1.isEqual(date2)}") // false
}
```
קטע הקוד הזה ידפיס:
```
Is date1 before date2? true
Is date1 after date2? false
Is date1 equal to date2? false
```

## עיון מעמיק:
השהוואה של שתי תאריכים היא פעולה קריטית שהייתה חלק מליבת פיתוח תוכנה מאז הימים הראשונים. בעבר השתמשו בפונקציות כמו `before()`, `after()` ו- `equals()` מכיתות כמו `java.util.Date` או `java.util.Calendar`. בגרסאות החדשות של ה-Java SE, אנו משתמשים ב-API של `java.time`, המכונה גם JSR-310. חלק מהיתרונות הכלולים בספרייה זו הם נוחות גבוהה יותר למשתמש, זמינות לאימות תקינות מדויק יותר ותמיכה באיזורי זמן.

## ראה גם:
- [The Java™ Tutorials - Date Time](https://docs.oracle.com/javase/tutorial/datetime/index.html)
- [Baeldung - Working with Date and Time in Kotlin](https://www.baeldung.com/kotlin-dates)
