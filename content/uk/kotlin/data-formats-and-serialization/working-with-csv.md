---
title:                "Робота з CSV"
aliases: - /uk/kotlin/working-with-csv.md
date:                  2024-02-03T19:20:38.529038-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з CSV (значення, розділені комами) включає в себе читання з файлів CSV та запис даних у файлах CSV, що є поширеним форматом для зберігання табличних даних у вигляді звичайного тексту. Програмісти маніпулюють файлами CSV для легкого обміну даними між різними додатками, базами даних або для спрощення завдань обробки та аналізу даних.

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
