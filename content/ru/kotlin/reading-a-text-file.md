---
title:                "Чтение текстового файла"
date:                  2024-01-29T00:00:47.877377-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Чтение текстового файла означает извлечение данных из файла в вашу программу, обычно построчно. Программисты делают это для обработки или анализа данных, которые хранятся внешне.

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
