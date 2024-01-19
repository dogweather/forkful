---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה? 

קריאת קובץ טקסט היא פעולה שבמה המחשב מקבל את הנתונים מתוך קובץ טקסט ומציג אותם למתכנת. תוכניתים מבצעות את הפעולה הזו כדי לטפל בנתונים ולבצע פעולות על פי התוצאות.

## איך לעשות:

במקסם שהקוד שבכלים קוטלין יכולה לבצע קריאה של קובץ טקסט וזה מאוד פשוט. מספר שורות של קוד צריך לעשות את העבודה.

```Kotlin
import java.io.File

fun main() {
    val content = File("example.txt").readText()
    println(content)
}
```
במשתנה תוכן, יהיה מכיל את כל התוכן של קובץ אקסמפל.טקסט שאפשר להדפיסו או לבצע פעולות אחרות נוותות.

## צלילה עמוקה 

מאז השנים המוקדמות של התכנות, היה כמעט תמיד צורך לקרוא מקבצים טקסט. זה אחד הכלים הבסיסיים של מתכנת והוא משמש לשלל מטרות, משמירה על היסטוריה של הנתונים, בניהן מסדי נתונים. 

ייתכן שישנן חלופות לקריאת קבצי טקסט בלוקחין, כולל קריאה ממסדי נתונים או שימור מידע בזיכרון הרציה של התוכנית. אך כניסה אל קבצי טקסט עדיין נחשבת לדרך המקובלת והנפוצה.

## ראה גם 

1. איך להשתמש בקורא תווים בקוטלין: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.nio.file.-path/read-text.html
2. פונקציות File IO בקוטלין: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html
3. פונקציות InputStream בקוטלין: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.nio.file.-path/read-lines.html