---
title:                "חישוב תאריך בעתיד או בעבר"
aliases:
- he/kotlin/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:55.807400-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פעולה שבה אנחנו מוסיפים או מחסירים ימים, חודשים, או שנים מתאריך מסוים. תכנותים עושים את זה לתכנון משימות, גביית תשלומים, הערכת מועדי אספקה, ועוד.

## איך לעשות:
עבור חישובי תאריכים בקוטלין, אפשר להיעזר בספריית java.time (מ-Java 8 ואילך). להלן דוגמה:

```kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val today = LocalDate.now()
    val nextWeek = today.plus(1, ChronoUnit.WEEKS)
    val threeMonthsAgo = today.minus(3, ChronoUnit.MONTHS)

    println("Today: $today")
    println("Next Week: $nextWeek")
    println("Three Months Ago: $threeMonthsAgo")
}
```

פלט לדוגמה:
```
Today: 2023-03-15
Next Week: 2023-03-22
Three Months Ago: 2022-12-15
```

## עיון מעמיק:
בעבר, הספריה הפופולרית ביותר עבור תאריכים הייתה `java.util.Date` ו-`Calendar`, אך הם נחשבו למבולגנים ולא נוחים. עם פיתוח Java 8, הספרייה `java.time` הוצגה, והפכה לסטנדרט הנכון לעבודה עם זמנים ותאריכים. בכל הקשור לקוטלין, ספריית java.time משתלבת נהדרת בקוד ומספקת פונקציונליות חזקה ומדויקת. לחלופין, ניתן גם להשתמש בספריות חיצוניות כגון Joda-Time עד שהממשק של java.time נכנס לשימוש נרחב. עם זאת, לרוב בנית האפליקציות המודרניות יש יתרון להיצמד לספריית java.time בשל התמיכה הרשמית והעדכניות.

## ראה גם:
- [Java 8 Date Time API Guide](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html) – מדריך לספריית java.time של Java 8.
- [Kotlin Documentation - Using Java 8 Date and Time API](https://kotlinlang.org/docs/java-interop.html#java-8-date-and-time-api) – התיעוד הרשמי של קוטלין לשימוש ב-Java 8 Date and Time API.
- [Threeten Backport Project](https://www.threeten.org/threetenbp/) – פורט עבור Java 6 ו-7 של ספריית java.time, לשימוש עד שהאפליקציה שלך תוכל להיעזר ב-Java 8.
