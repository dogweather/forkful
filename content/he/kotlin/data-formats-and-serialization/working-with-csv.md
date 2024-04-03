---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:50.617928-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD CSV (Comma-Separated Values\
  \ \u05D0\u05D5 \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\
  \u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05EA\u05D5\u05DA \u05E7\u05D1\u05E6\u05D9\
  \u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 CSV \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4\
  \ \u05D0\u05DC\u05D9\u05D4\u05DD, \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05E4\u05D5\
  \u05E5 \u05DC\u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD \u05D1\u05D8\u05E7\u05E1\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.309884-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD CSV (Comma-Separated Values\
  \ \u05D0\u05D5 \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\
  \u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05EA\u05D5\u05DA \u05E7\u05D1\u05E6\u05D9\
  \u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 CSV \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4\
  \ \u05D0\u05DC\u05D9\u05D4\u05DD, \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05E4\u05D5\
  \u05E5 \u05DC\u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD \u05D1\u05D8\u05E7\u05E1\u05D8 \u05E4\
  \u05E9\u05D5\u05D8."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

## מה ולמה?

עבודה עם CSV (Comma-Separated Values או ערכים מופרדים בפסיקים) כוללת קריאה מתוך קבצים בפורמט CSV וכתיבה אליהם, פורמט נפוץ לאחסון נתונים טבלאיים בטקסט פשוט. מתכנתים מתעסקים בקבצי CSV כדי להחליף נתונים בין יישומים שונים, מסדי נתונים, או לסייע במשימות של עיבוד וניתוח נתונים בקלות.

## איך לעשות:

Kotlin, שהיא שפת תכנות סטטית שמתבצעת על חלקת ה-JVM, אינה כוללת ספריה מובנית לעבודה עם קבצי CSV. עם זאת, ניתן להשתמש במחלקות `BufferedReader` ו-`FileWriter` של Java לפעולות בסיסיות, או לנצל ספריות צד שלישי פופולריות כמו `kotlinx.serialization` ו-`opencsv` לפונקציונליות מתקדמת יותר.

### קריאה מקובץ CSV באמצעות BufferedReader:

```kotlin
import java.io.BufferedReader
import java.io.FileReader

fun main() {
    val path = "data.csv"
    val br = BufferedReader(FileReader(path))
    br.useLines { lines ->
        lines.forEach { line ->
            val cols = line.split(',')
            println(cols)
        }
    }
}
```

_דוגמא לפלט:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### כתיבה לקובץ CSV באמצעות FileWriter:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Name", "Age", "City"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "London")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

הדבר יוצר או דורס את הקובץ `output.csv` עם הנתונים שסופקו.

### שימוש ב-kotlinx.serialization לסריאליזציה של CSV:

תחילה, הוסף את התלות ל-`build.gradle.kts` שלך:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_הערה: הקפד לציין גרסה נכונה ותצורת מאגר._

לאחר מכן, הגדר מחלקת נתונים והשתמש בפורמט `Csv` לסריאליזציה:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Person(val name: String, val age: Int, val city: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        Person("John Doe", 30, "New York"),
        Person("Jane Smith", 25, "London")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_דוגמא לפלט:_

```
John Doe,30,New York
Jane Smith,25,London
```

### שימוש ב-OpenCSV לפעולות מתקדמות:

הוסף את OpenCSV לתלותיות של הפרויקט שלך:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

קריאה וכתיבה עם OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // קריאת CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // כתיבת CSV
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val entries = listOf(
            arrayOf("Name", "Age", "City"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        csvWriter.writeAll(entries)
    }
}
```

הדוגמאות הללו מדגימות את הגמישות ש-Kotlin מציעה בעבודה עם קבצי CSV, ומאפשרת לך לבחור את השיטה המתאימה ביותר לצרכי הפרויקט שלך.
