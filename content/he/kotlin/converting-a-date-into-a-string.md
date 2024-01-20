---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיבור תאריך למחרוזת הוא התהליך של המרת ערך תאריך למחרוזת. מתכנתים מבצעים זאת כדי להציג נתונים בצורה אנושית יותר או לשמור תאריך במערכת אחסון נתונים.

## איך לעשות את זה:
הקטע הבא מהווה דוגמה לקוד קוטלין שממיר תאריך למחרוזת:
```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val rawDate = Date()
    val dateFormat = SimpleDateFormat("dd-MM-yyyy")
    val dateString = dateFormat.format(rawDate)

    println(dateString)
}
```
בתוצאה, תקבלו את התאריך של היום בפורמט "DD-MM-YYYY" והוא יהיה מודפס על המסך.

## צלילה עמוקה:
נמירת תאריכים למחרוזות הופכת להיות דרושה יותר ביישומים מודרניים שמשתמשים במסדי נתונים או API. כל סביבת עבודה מחייבת שורת קוד מסוימת למרת תאריך למחרוזת.

לחלופין, אם אתה משתמש במסדי נתונים כמו SQL, ייתכן ותרצה לשמור את התאריך כדי לשמור על יעילות הביצועים. ליישומים כאלה, היית צריך להמיר את המחרוזת חזרה לתאריך.

`SimpleDateFormat` הוא מחלקה בחבילת `java.text` שמשמשת לפורמט ולניתוח תאריכים בצורה חובה. זה מתיחס לתאריך כתובת רגילה של תווים שמורכבים ממרכיבים שונים של התאריך, כמו "שנה", "חודש", "יום", "שעה", וכו'.

## ראה גם:
* [תיעוד של SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
* [הסבר על פורמט תאריך בקוטלין באתר StackOverflow](https://stackoverflow.com/questions/43339298/how-to-format-date-and-time-in-kotlin)