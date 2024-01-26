---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לקובץ טקסט בקוטלין היא פעולה של שמירת נתונים בקובץ על מנת שיהיו נגישים לעתיד. תוכניתנים עושים זאת כדי לאחסן הגדרות, לוגים, נתונים לעיבוד נוסף, או לשתף מידע.

## כיצד לעשות:

```Kotlin
import java.io.File

fun main() {
    val textToWrite = "שלום, זה טקסט בעברית!"
    File("output.txt").writeText(textToWrite, Charsets.UTF_8)
    println("טקסט נשמר בהצלחה בקובץ 'output.txt'.")
}
```

פלט לדוגמא:
```
טקסט נשמר בהצלחה בקובץ 'output.txt'.
```

## עיון עמוק:
כתיבה לקובץ היא טכניקה שנמצאת בשימוש מתחילת ימי המחשבים. יש דרכים חלופיות כמו שימוש ב-database או טכנולוגיות ענן, אך כתיבה לקובץ נשארת נפוצה בגלל פשטותה. בקוטלין, ניתן לכתוב לקבצים באמצעות המחלקה 'File' מהספרייה הסטנדרטית של ג'אווה, או באמצעות מחלקות וספריות חיצוניות לשיפור אבטחה ונוחות.

## ראה גם:
- [Official Kotlin documentation on File I/O](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- [Kotlin API reference for File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
