---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV (ערכים מופרדים בפסיקים) היא טיפול בנתונים טבולריים בצורה טקסטואלית. תכנתים עושים זאת כדי לייבא, לעבד ולייצא נתונים בפורמט שנתמך על ידי מגוון כלים.

## איך לעשות:
קריאה מCSV:
```kotlin
import java.io.File

fun main() {
    val csvFile = File("data.csv")
    csvFile.forEachLine { line ->
        val columns = line.split(",")
        println(columns)
    }
}
```
פלט לדוגמא:
```
[שם, גיל, מגורים]
[דני, 35, תל אביב]
[אורית, 29, ירושלים]
```
כתיבה לCSV:
```kotlin
import java.io.File

fun main() {
    val data = listOf(listOf("שם", "גיל", "מגורים"), listOf("דני", "35", "תל אביב"), listOf("אורית", "29", "ירושלים"))
    File("output.csv").printWriter().use { out ->
        data.forEach { record ->
            out.println(record.joinToString(","))
        }
    }
}
```

## צלילה לעומק
CSV הוא פורמט נתונים פשוט שהחל את דרכו בשנות ה-70. ישנם אלטרנטיבות כמו JSON וXML שמציעות יותר גמישות בביטוי מבנים נתונים מורכבים. בכתיבת קוד לעבודה עם CSV יש לטפל בבעיות כמו שדות עם פסיקים או שורות חדשות בתוכם, מה שדורש עטיפת השדה במרכאות.

## ראה גם
- ספריית [kotlin-csv](https://github.com/doyaaaaaken/kotlin-csv): ספריית CSV עבור Kotlin שפותחה לנוחות עבודה עם פורמט זה.
- מדריך ל[Java CSV פרסר](https://www.baeldung.com/opencsv): למרות שהוא ל-Java, המדריך עוזר למי שעובד עם קוטלין ורוצה להבין גישות לעבודה עם CSV.
