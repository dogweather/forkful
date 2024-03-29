---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:47.428285-07:00
description: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432 \u043F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0438 \u0432\
  \u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0441\u043E\u0437\u0434\u0430\u043D\u0438\
  \u0435 \u0438 \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\u0438\u0435 \u0434\
  \u0430\u043D\u043D\u044B\u0445 \u0432 \u0447\u0438\u0442\u0430\u0435\u043C\u043E\
  \u043C \u0444\u043E\u0440\u043C\u0430\u0442\u0435 \u0444\u0430\u0439\u043B\u0430\
  , \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440, .txt. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:45.012904-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441\u044C \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432 \u043F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0438 \u0432\
  \u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0441\u043E\u0437\u0434\u0430\u043D\u0438\
  \u0435 \u0438 \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\u0438\u0435 \u0434\
  \u0430\u043D\u043D\u044B\u0445 \u0432 \u0447\u0438\u0442\u0430\u0435\u043C\u043E\
  \u043C \u0444\u043E\u0440\u043C\u0430\u0442\u0435 \u0444\u0430\u0439\u043B\u0430\
  , \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440, .txt. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E \u0434\u043B\u044F\u2026"
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
---

{{< edit_this_page >}}

## Что и Почему?

Запись текстового файла в программировании включает создание и сохранение данных в читаемом формате файла, например, .txt. Программисты делают это для сохранения данных, конфигурации систем, ведения журналов информации или экспорта информации в удобочитаемом виде.

## Как это сделать:

Давайте напишем "Привет, файл!" в файл "greeting.txt".

```Kotlin
import java.io.File

fun main() {
    val textToWrite = "Привет, файл!"
    File("greeting.txt").writeText(textToWrite)
}
```

После выполнения:
```
Привет, файл! (в greeting.txt)
```

Что делать, если нам нужно добавить текст, а не перезаписать?

```Kotlin
fun appendTextToFile(filename: String, text: String) {
    File(filename).appendText("\n$text")
}

fun main() {
    appendTextToFile("greeting.txt", "Ещё одна строка!")
}
```

Результат в `greeting.txt`:
```
Привет, файл!
Ещё одна строка!
```

## Подробнее

Исторически текстовые файлы были краеугольным камнем в настройке и ведении журналов в системах программного обеспечения. Хотя инструменты и форматы (такие как XML, JSON) эволюционировали, текстовые файлы остаются простым и универсально доступным методом взаимодействия с данными.

Альтернативы `java.io.File` включают `java.nio.file.Files` и `java.io.FileWriter`, предлагающие больше контроля и эффективности для работы с большими файлами или более сложными операциями.

Ключевые детали реализации:
- **Кодировка**: По умолчанию, `writeText` использует кодировку UTF-8. Для другой кодировки используйте `writeText(textToWrite, Charsets.ISO_8859_1)` или что-то подобное.
- **Буферизация**: При работе с большими файлами не забывайте о буферизации. Оберните ваш писатель в `BufferedWriter` для лучшей производительности.
- **Обработка Исключений**: Будьте внимательны к потенциальным `IOExceptions` и обрабатывайте их соответствующим образом.

## Смотрите также

- Официальная документация Kotlin по работе с файлами: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- Пакет `java.nio.file` для современной файловой ввода-вывода: [Java Docs](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)
- Узнайте о `BufferedWriter` для эффективной записи: [Java BufferedWriter](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)
