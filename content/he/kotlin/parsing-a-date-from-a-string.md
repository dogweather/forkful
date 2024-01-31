---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:37:31.699309-07:00
simple_title:         "ניתוח תאריך ממחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח תאריך ממחרוזת מדובר בהמרה של טקסט לפורמט תאריך שהמחשב מבין. תוכניתנים עושים זאת כדי לנהל נתונים תאריכיים ביעילות, לאחסן במסדי נתונים או להציג באופן נכון למשתמשים.

## איך לעשות:
בקוטלין, תוכלו להשתמש ב-mport `java.time.format.DateTimeFormatter` וב-`java.time.LocalDate` לפיענוח תאריכים:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDate {
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    return LocalDate.parse(dateString, formatter)
}

fun main() {
    val dateText = "15/04/2023"
    val parsedDate = parseDateFromString(dateText)
    println("תאריך מפוענח: $parsedDate")
}
```

פלט לדוגמה:
```
תאריך מפוענח: 2023-04-15
```

## עיון מעמיק:
בעבר, תוכניתנים השתמשו ב-`java.util.Date` או ב-`java.text.SimpleDateFormat`, אבל מאז גרסת ג'אווה 8 וכן בקוטלין, העדיפות היא ל-API של `java.time`, שקל יותר לשימוש וחסין יותר לשגיאות.
אלטרנטיבות כוללות ספריות חיצוניות כמו Joda-Time, אך לרוב אין צורך בהן כיום.

פרטי מימוש: כאשר אנו מפענחים תאריך, יש להקפיד על פורמט המחרוזת. אם הפורמט שונה, יש לשנות את `DateTimeFormatter` בהתאמה. פעולה זו רגישה לשגיאות כמו תאריכי לא קיימים או פורמט לא תקין.

## גם כן ראה:
- מדריך ל-API של `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- DateTimeFormatter תיעוד: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- על `SimpleDateFormat` ובעיותיו: https://www.baeldung.com/java-simpledateformat
- ספריית Joda-Time: https://www.joda.org/joda-time/
