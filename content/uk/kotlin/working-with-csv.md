---
title:                "Kotlin: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Для чого

Робота з CSV файлами є важливою частиною програмування, особливо для людей, які працюють з великою кількістю даних. CSV - це текстовий формат, який дозволяє зберігати й організовувати дані у вигляді таблиць. Навчитися працювати з CSV файлами дуже корисно для ведення обліку, аналізу даних, створення звітів та багатьох інших сфер програмування.

## Як

Для роботи з CSV файлами у Kotlin можна використовувати бібліотеку kotlin-csv, яка надає зручний і простий інтерфейс для читання та запису даних. Давайте подивимося на приклад коду, який дозволить нам зчитати дані з CSV файлу і вивести їх на екран:

```Kotlin
val file = File("data.csv").inputStream()
val rows = file.reader().readAll()
for (row in rows) {
    for (value in row) {
        print("$value\t")
    }
    println()
}
```

Вивід:

```
Name        Age        City
John        35         New York
Maria       28         London
Ivan        42         Kiev
```

Також можна застосовувати фільтри та сортування до даних з CSV файлу, використовуючи функції бібліотеки. Розглянемо приклад коду, який виведе дані лише тих людей, які вже народилися:

```Kotlin
val file = File("data.csv").inputStream()
val rows = file.reader().readAll()
val filteredRows = rows.filter { it[1].toInt() > 0 } // фільтруємо за віком
filteredRows.sortedBy { it[1].toInt() } // сортуємо за віком
for (row in filteredRows) {
    for (value in row) {
        print("$value\t")
    }
    println()
}
```

Вивід:

```
Name        Age        City
Maria       28         London
John        35         New York
Ivan        42         Kiev
```

## Глибока погрузка

Якщо ви хочете поглибитися у роботу з CSV файлами, бібліотека kotlin-csv надає багато можливостей для маніпулювання даними. За допомогою функцій каррінгу та пайплайнів можна зчитувати, фільтрувати, сортувати, об'єднувати та групувати дані з CSV файлу у більш складні способи. Також можна зберігати дані у CSV файл з різним роздільником та експортувати дані до Excel або інших програм.

Більше інформації про бібліотеку kotlin-csv та роботу з CSV файлами можна знайти у [офіційній документації](https://github.com/doyaaaaaken/kotlin-csv) та [репозиторії на GitHub](https://github.com/doyaaaaaken/kotlin-csv).

## Дивіться також

- [Офіційна документація по Kotlin](https