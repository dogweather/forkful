---
date: 2024-01-20 17:33:47.038634-07:00
description: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D6\u05D4\u05D5 \u05E4\u05E2\u05D5\u05DC\u05D4\
  \ \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D5 \u05D1\u05D5\u05D3\u05E7\u05D9\u05DD \u05D4\
  \u05D0\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\u05D7\u05D3 \u05D4\u05D5\u05D0\
  \ \u05DC\u05E4\u05E0\u05D9, \u05D0\u05D7\u05E8\u05D9 \u05D0\u05D5 \u05D6\u05D4\u05D4\
  \ \u05DC\u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\u05D7\u05E8. \u05EA\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\
  \u05DC \u05DE\u05E0\u05EA \u05DC\u05DE\u05D9\u05D9\u05DF \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05EA\
  \u05E7\u05D9\u05E0\u05D5\u05EA, \u05D0\u05D5\u2026"
lastmod: '2024-02-25T18:49:37.533137-07:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05D6\u05D4\u05D5 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\
  \u05D1\u05D4 \u05D0\u05E0\u05D5 \u05D1\u05D5\u05D3\u05E7\u05D9\u05DD \u05D4\u05D0\
  \u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\u05D7\u05D3 \u05D4\u05D5\u05D0 \u05DC\
  \u05E4\u05E0\u05D9, \u05D0\u05D7\u05E8\u05D9 \u05D0\u05D5 \u05D6\u05D4\u05D4 \u05DC\
  \u05EA\u05D0\u05E8\u05D9\u05DA \u05D0\u05D7\u05E8. \u05EA\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC\
  \ \u05DE\u05E0\u05EA \u05DC\u05DE\u05D9\u05D9\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD, \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05EA\u05E7\
  \u05D9\u05E0\u05D5\u05EA, \u05D0\u05D5\u2026"
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
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
