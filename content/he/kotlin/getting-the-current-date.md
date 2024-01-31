---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:15:59.459846-07:00
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
להשיג את התאריך הנוכחי בקוד משמעו לקבל את התאריך והשעה בזמן אמיתי בו הקוד רץ. מתכנתים עושים זאת ללוגים, תיעוד תהליכים, תזמונים ועוד.

## איך לעשות:
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    // קבלת התאריך הנוכחי
    val today = LocalDate.now()
    println("התאריך הנוכחי: $today")

    // פורמט מותאם אישית של התאריך
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    val formattedDate = today.format(formatter)
    println("התאריך הנוכחי בפורמט dd/MM/yyyy: $formattedDate")
}
```
פלט דוגמא (ישתנה בהתאם ליום הריצה של הקוד):
```
התאריך הנוכחי: 2023-04-12
התאריך הנוכחי בפורמט dd/MM/yyyy: 12/04/2023
```

## צלילה עמוקה
המודול `java.time.LocalDate` הוצג ב-Java 8 והוא חלק מה-Java Date and Time API החדשני, שתכננו לתת פתרון לחסרונות ב-API הקודם (`java.util.Date`). זה נותן לנו מטרה ממוקדת של תאריך בלי זמן ואיזור זמן. אלטרנטיבות כוללות את השימוש ב-`java.util.Calendar` או ספריות חיצוניות כמו Joda-Time, אבל `LocalDate` הוא הבחירה המועדפת מאז Java 8 כי הוא בלתי נתון לשינויים (immutable), ברור וידידותי למשתמש. כשאנחנו קוראים `LocalDate.now()`, זה למעשה שואל את השעון המערכתי על התאריך הנתון לפי איזור הזמן של המחשב שהתכנית פועלת עליו.

## ראה גם
- Java Date and Time API: https://docs.oracle.com/javase/tutorial/datetime/iso/
- Joda-Time – הייתה אלטרנטיבה נפוצה לטיפול בתאריכים ובזמנים לפני Java 8: https://www.joda.org/joda-time/
- על mutable ו-immutable objects בג'אווה: https://docs.oracle.com/javase/tutorial/essential/concurrency/immutable.html
