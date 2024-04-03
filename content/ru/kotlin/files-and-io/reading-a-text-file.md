---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:47.877377-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Kotlin \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u043B\
  \u0435\u0433\u043A\u043E \u043F\u0440\u043E\u0447\u0438\u0442\u0430\u0442\u044C\
  \ \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0439 \u0444\u0430\u0439\u043B\
  , \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044F \u0444\u0443\u043D\u043A\
  \u0446\u0438\u044E `readLines()` \u0438\u043B\u0438 \u0431\u043B\u043E\u043A `useLines`."
lastmod: '2024-03-13T22:44:45.011076-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Kotlin \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u043B\u0435\
  \u0433\u043A\u043E \u043F\u0440\u043E\u0447\u0438\u0442\u0430\u0442\u044C \u0442\
  \u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0439 \u0444\u0430\u0439\u043B, \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044F \u0444\u0443\u043D\u043A\u0446\u0438\
  \u044E `readLines()` \u0438\u043B\u0438 \u0431\u043B\u043E\u043A `useLines`."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 22
---

## Как это сделать:
В Kotlin вы можете легко прочитать текстовый файл, используя функцию `readLines()` или блок `useLines`.

```Kotlin
import java.io.File

fun main() {
    // Чтение всех строк сразу
    val lines = File("example.txt").readLines()
    lines.forEach { line ->
        println(line)
    }

    // Более эффективно для больших файлов
    File("example.txt").useLines { lines ->
        lines.forEach { line ->
            println(line)
        }
    }
}
```

Пример вывода (предполагая, что `example.txt` содержит две строки с "Hello" и "World"):

```
Hello
World
```

## Подробнее
Исторически сложилось так, что чтение файлов в Java могло быть многословным и неуклюжим. С Kotlin стандартная библиотека предоставляет удобные расширения, чтобы сделать чтение файлов проще.

Есть альтернативы для чтения файлов в Kotlin:
1. `readText()` читает весь контент файла в `String`.
2. `bufferedReader()` предоставляет `BufferedReader`, который позволяет решать более сложные задачи, например, чтение огромных файлов без излишнего потребления памяти.

С точки зрения реализации, когда вы используете `useLines`, это обеспечивает закрытие файла после выполнения, предотвращая потенциальные утечки памяти. Это функциональный подход, который рекомендуется в Kotlin для эффективного управления ресурсами.

## См. также
- Документация Kotlin по чтению файлов: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- Документация `BufferedReader` для более сложных случаев: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/)
