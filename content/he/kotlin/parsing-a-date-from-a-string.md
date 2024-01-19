---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# פירוק תאריך ממחרוזת ב-Kotlin
## ?מה ולמה
פענוח תאריך ממחרוזת הוא תהליך שבו אנחנו משנים מחרוזת שמכילה תאריך לאובייקט תאריך. מתכנתים עושים זאת לנוחיות עיבוד, אחסון, והצגה של תאריכים.

## כיצד לעשות
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2022-03-02"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date = LocalDate.parse(dateString, formatter)
    println(date)
}
```

פלט
```
2022-03-02
```

## צלילה עמוקה
- היסטוריה: הפארסינג של תאריכים ממחרוזת האם בוצע באמצעות יישומי מוח מורכבים, אך בעזרת Kotlin זה נהיה די פשוט.
- אלטרנטיבות: ישנן ספריות נוספות כמו Joda-Time וDate4J שיכולות לבצע פעולות דומות.
- פירוט:הפעולה `LocalDate.parse()` מקבלת מחרוזת תאריך ומבצעת פירוק לפי הפורמט שהוכנס דרך `DateTimeFormatter`.

## ראה גם
- [תיעוד Kotlin רשמי](https://kotlinlang.org/docs/reference/)
- [תיעוד Java Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [מדריך למחרוזות תאריך ושעה](https://www.javatpoint.com/java-date-string)