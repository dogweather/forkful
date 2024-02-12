---
title:                "עובדים עם CSV"
aliases:
- /he/kotlin/working-with-csv/
date:                  2024-02-03T19:20:50.617928-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
