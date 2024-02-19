---
aliases:
- /uk/kotlin/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:38.529038-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV (\u0437\u043D\u0430\u0447\
  \u0435\u043D\u043D\u044F, \u0440\u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456\
  \ \u043A\u043E\u043C\u0430\u043C\u0438) \u0432\u043A\u043B\u044E\u0447\u0430\u0454\
  \ \u0432 \u0441\u0435\u0431\u0435 \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0437\
  \ \u0444\u0430\u0439\u043B\u0456\u0432 CSV \u0442\u0430 \u0437\u0430\u043F\u0438\
  \u0441 \u0434\u0430\u043D\u0438\u0445 \u0443 \u0444\u0430\u0439\u043B\u0430\u0445\
  \ CSV, \u0449\u043E \u0454 \u043F\u043E\u0448\u0438\u0440\u0435\u043D\u0438\u043C\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0434\u043B\u044F \u0437\u0431\
  \u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F\u2026"
lastmod: 2024-02-18 23:09:00.306655
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV (\u0437\u043D\u0430\u0447\
  \u0435\u043D\u043D\u044F, \u0440\u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456\
  \ \u043A\u043E\u043C\u0430\u043C\u0438) \u0432\u043A\u043B\u044E\u0447\u0430\u0454\
  \ \u0432 \u0441\u0435\u0431\u0435 \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0437\
  \ \u0444\u0430\u0439\u043B\u0456\u0432 CSV \u0442\u0430 \u0437\u0430\u043F\u0438\
  \u0441 \u0434\u0430\u043D\u0438\u0445 \u0443 \u0444\u0430\u0439\u043B\u0430\u0445\
  \ CSV, \u0449\u043E \u0454 \u043F\u043E\u0448\u0438\u0440\u0435\u043D\u0438\u043C\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0434\u043B\u044F \u0437\u0431\
  \u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
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
