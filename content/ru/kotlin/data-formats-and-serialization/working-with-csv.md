---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:30.805823-07:00
description: "\u041A\u0430\u043A: \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\
  \u043E\u0442\u0430\u0442\u044C \u0441 CSV \u0432 Kotlin, \u0432\u044B \u043C\u043E\
  \u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u0442\u044C \u0431\u0430\u0437\u043E\u0432\u0443\u044E \u0431\u0438\u0431\u043B\
  \u0438\u043E\u0442\u0435\u043A\u0443 \u0438\u043B\u0438 \u0441\u0442\u043E\u0440\
  \u043E\u043D\u043D\u0438\u0435 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\
  \u0438, \u0442\u0430\u043A\u0438\u0435 \u043A\u0430\u043A Kotlinx.serialization\
  \ \u0438\u043B\u0438 Apache Commons\u2026"
lastmod: '2024-03-13T22:44:45.020006-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0442\
  \u044C \u0441 CSV \u0432 Kotlin, \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\
  \u0430\u0437\u043E\u0432\u0443\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0443 \u0438\u043B\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0438\
  \u0435 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\
  \u043A\u0438\u0435 \u043A\u0430\u043A Kotlinx.serialization \u0438\u043B\u0438 Apache\
  \ Commons CSV."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как:
Чтобы работать с CSV в Kotlin, вы можете использовать базовую библиотеку или сторонние библиотеки, такие как Kotlinx.serialization или Apache Commons CSV. Здесь я покажу вам основные операции ввода/вывода без внешних библиотек.

```kotlin
import java.io.File

fun main() {
    // Запись в CSV
    val outputFile = File("data.csv")
    outputFile.printWriter().use { out ->
        out.println("id,name,age")
        out.println("1,John Doe,30")
        out.println("2,Jane Smith,25")
    }

    // Чтение из CSV
    File("data.csv").forEachLine { line ->
        val (id, name, age) = line.split(',')
        println("ID: $id, Имя: $name, Возраст: $age")
    }
}
```

Вывод:
```text
ID: 1, Имя: John Doe, Возраст: 30
ID: 2, Имя: Jane Smith, Возраст: 25
```

## Подробнее
Корни CSV уходят в ранние дни компьютерной эры, когда память была ограничена, и форматы обмена данными должны были быть простыми. Несмотря на появление альтернатив, таких как JSON и XML, CSV остается популярным благодаря своей простоте использования, совместимости и читаемости для человека.

Корректная обработка CSV может быть более сложной из-за особых случаев (например, запятые в данных, многострочные поля и т.д.). Библиотеки, такие как Apache Commons CSV и Kotlinx.serialization, учитывают эти случаи и предоставляют дополнительный функционал.

## Смотрите также
- [RFC 4180](https://tools.ietf.org/html/rfc4180): Общий формат и MIME-тип для файлов CSV.
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/): Java-библиотека для работы с файлами CSV, которую можно использовать в Kotlin.
- [Kotlinx.serialization CSV](https://github.com/Kotlin/kotlinx.serialization): Kotlin-библиотека, упрощающая сериализацию в формат и из формата CSV.
