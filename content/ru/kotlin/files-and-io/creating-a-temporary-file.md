---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:06.058096-07:00
description: "\u041A\u0430\u043A: \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\
  \u044B\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u0441\u043E\u0437\u0434\u0430\
  \u0442\u044C \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0439 \u0444\u0430\
  \u0439\u043B \u043D\u0430 Kotlin."
lastmod: '2024-03-13T22:44:45.014708-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u0441\u043F\
  \u043E\u0441\u043E\u0431 \u0441\u043E\u0437\u0434\u0430\u0442\u044C \u0432\u0440\
  \u0435\u043C\u0435\u043D\u043D\u044B\u0439 \u0444\u0430\u0439\u043B \u043D\u0430\
  \ Kotlin."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0432\u0440\u0435\u043C\u0435\
  \u043D\u043D\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 21
---

## Как:
Вот быстрый способ создать временный файл на Kotlin:

```Kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile("myTempFile", ".tmp")

    println("Временный файл создан по адресу: ${tempFile.absolutePath}")

    // Запись во временный файл
    tempFile.writeText("Kotlin довольно удивительный, правда?")

    // Удаление при выходе
    tempFile.deleteOnExit()
}
```

Вывод будет примерно такой:

```
Временный файл создан по адресу: /tmp/myTempFile1234567890.tmp
```

Путь к вашему временному файлу будет отличаться. У него будет уникальное имя, так что не волнуйтесь о конфликтах имен.

## Глубокое погружение
Метод `File.createTempFile()` является золотым стандартом для создания файлов ad-hoc. Он существует с начала дней Java, и Kotlin, как язык JVM, полностью использует его преимущества.

Некоторые альтернативы:
- `Files.createTempFile()` из `java.nio.file` предлагает больше контроля, например, установку атрибутов файла.
- В памяти базы данных или кэши могли бы заменить временные файлы для некоторых случаев использования (например, `H2` или `Redis`).

По умолчанию, временные файлы хранятся в системной директории для временных файлов по умолчанию, но вы можете указать свой путь. Помните о том, чтобы подчищать за собой; временные файлы не гарантированно будут удалены после выполнения вашей программы. Метод `deleteOnExit()` гарантирует, что файл будет удален при завершении работы JVM, но это не является надежным для долгосрочных приложений.

## Смотрите также
Больше о временных файлах на Kotlin и Java:
- Официальная документация по `File` на Kotlin: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- Класс `File` в Java: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- Для более глубокого понимания атрибутов файла: [https://docs.oracle.com/javase/tutorial/essential/io/fileAttr.html](https://docs.oracle.com/javase/tutorial/essential/io/fileAttr.html)
