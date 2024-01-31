---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Що це таке та Навіщо?
Обробка CSV (Comma-Separated Values) файлів це читання та запис даних у форматі, де значення розділені комами. Програмісти використовують CSV через простоту і універсальність – цей формат легко імпортувати в таблиці, і він підтримується більшістю програм.

## Як це зробити:
```Kotlin
import java.io.File

fun readCsv(filePath: String): List<List<String>> {
    return File(filePath).useLines { lines ->
        lines.map { it.split(",") }.toList()
    }
}

fun writeCsv(filePath: String, data: List<List<String>>) {
    File(filePath).bufferedWriter().use { out ->
        data.forEach { line ->
            out.write(line.joinToString(","))
            out.newLine()
        }
    }
}

// Читання CSV
val csvData = readCsv("data.csv")
println(csvData)

// Запис у CSV
val newData = listOf(listOf("id", "name", "email"), listOf("1", "John Doe", "john@example.com"))
writeCsv("new_data.csv", newData)
```
```output
[[id, name, email], [1, John Doe, john@example.com]]
```

## Глибше занурення
CSV бере свій початок з ранніх версій електронних таблиць. Хоча JSON та XML можуть служити альтернативами для передачі даних, CSV залишається популярним завдяки своєй простоті і легкій інтеграції з таблицями і базами даних. При роботі з CSV важливо врахувати кодування файлу, розміщення рядків, і розмір файлу для ефективної обробки даних.

## Дивись також:
- Офіційну документацію Kotlin: https://kotlinlang.org/docs/home.html
- Репозиторій з бібліотекою для роботи з CSV в Kotlin: https://github.com/doyaaaaaken/kotlin-csv
- Введення у роботу з файлами у Kotlin: https://kotlinlang.org/docs/reading-writing-files.html
