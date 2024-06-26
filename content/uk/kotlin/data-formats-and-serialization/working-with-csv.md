---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:38.529038-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Kotlin, \u0431\
  \u0443\u0434\u0443\u0447\u0438 \u0441\u0442\u0430\u0442\u0438\u0447\u043D\u043E\
  \ \u0442\u0438\u043F\u0456\u0437\u043E\u0432\u0430\u043D\u043E\u044E \u043C\u043E\
  \u0432\u043E\u044E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\
  \u043D\u044F, \u044F\u043A\u0430 \u043F\u0440\u0430\u0446\u044E\u0454 \u043D\u0430\
  \ JVM, \u043D\u0435 \u043C\u0456\u0441\u0442\u0438\u0442\u044C \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u043A\u0438 \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437\
  \ \u0444\u0430\u0439\u043B\u0430\u043C\u0438 CSV. \u041E\u0434\u043D\u0430\u043A\
  \u2026"
lastmod: '2024-03-13T22:44:49.256529-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, \u0431\u0443\u0434\u0443\u0447\u0438 \u0441\u0442\u0430\u0442\u0438\
  \u0447\u043D\u043E \u0442\u0438\u043F\u0456\u0437\u043E\u0432\u0430\u043D\u043E\u044E\
  \ \u043C\u043E\u0432\u043E\u044E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\
  \u0432\u0430\u043D\u043D\u044F, \u044F\u043A\u0430 \u043F\u0440\u0430\u0446\u044E\
  \u0454 \u043D\u0430 JVM, \u043D\u0435 \u043C\u0456\u0441\u0442\u0438\u0442\u044C\
  \ \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0438 \u0434\u043B\u044F \u0440\u043E\u0431\
  \u043E\u0442\u0438 \u0437 \u0444\u0430\u0439\u043B\u0430\u043C\u0438 CSV."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

## Як зробити:
Kotlin, будучи статично типізованою мовою програмування, яка працює на JVM, не містить вбудованої бібліотеки для роботи з файлами CSV. Однак ви можете використовувати класи Java `BufferedReader` та `FileWriter` для базових операцій або використовувати популярні сторонні бібліотеки, такі як `kotlinx.serialization` та `opencsv`, для більш розширеного функціоналу.

### Читання файлу CSV за допомогою BufferedReader:
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

_Приклад виводу:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### Запис у файл CSV за допомогою FileWriter:
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

Це створить або перезапише `output.csv` з наданими даними.

### Використання kotlinx.serialization для серіалізації CSV:
Спочатку додайте залежність до свого `build.gradle.kts`:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Примітка: Переконайтеся, що у вас правильна версія та конфігурація репозиторію._

Потім визначте ваш клас даних і використовуйте формат `Csv` для серіалізації:

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

_Приклад виводу:_

```
John Doe,30,New York
Jane Smith,25,London
```

### Використання OpenCSV для розширених операцій:
Додайте OpenCSV до залежностей вашого проекту:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Читання та запис з OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // Читання CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // Запис CSV
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

Ці приклади коду демонструють гнучкість, яку Kotlin пропонує при роботі з файлами CSV, дозволяючи вам вибрати метод, який найкраще відповідає потребам вашого проекту.
