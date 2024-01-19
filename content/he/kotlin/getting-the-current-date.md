---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

לקבלת התאריך הנוכחי, זה להוציא מידע בנוגע לזמן הנוכחי בחלל, את ץום והתאריך שבו אנחנו נמצאים. נבנה כדי לעזור למתכנתים למדוד את הזמן, ליצור תיעוד זמני לעסקאות, או כל מטרה שמחייבת תאריך וזמן מדויק.

## איך לעשות:

בקוטלין, ישנן כמה שיטות לקבלת התאריך הנוכחי. נוכל להשתמש במחלקה LocalDate, LocalDateTime או ZonedDateTime.

```Kotlin
import java.time.LocalDateTime
fun main() {
    val current = LocalDateTime.now()
    println("התאריך והשעה הנוכחיים: " + current)
}
```
פלט:
```
התאריך והשעה הנוכחיים: 2022-03-10T15:45:20.556042
```

## צלילה עמוקה

מאז שקוטלין הוקם ב-2011, כולם הפקדו את הזמן בצורות שונות. מחלקת Java.util.Date הייתה ראשונה שהציעה את היכולת הזו אך הועקפה ב-2014 עם הכנסת Java.time, והמחלקות LocalDateTime, LocalDate, ו -ZonedDateTime.

האלטרנטיבות כוללות ספריות חיצוניות או הטמעת שיטות אישיות שמניהלות הקבלה של התאריך והשעה.

המחלקות מסופקות על ידי מערכת ההפעלה, כך שהתוצאות שלהן יכולות להשתנות בהתאם לזמן המערכת ולאזור הזמן של המשתמש.

## ראו גם

[מדריך מקיף לקוטלין](https://kotlinlang.org/docs/home.html)
[מערכת Java Time](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/time/package-summary.html) 
[המחלקה LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-local-date/)