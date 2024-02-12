---
title:                "Анализ даты из строки"
aliases:
- /ru/kotlin/parsing-a-date-from-a-string/
date:                  2024-01-29T00:00:19.676753-07:00
model:                 gpt-4-0125-preview
simple_title:         "Анализ даты из строки"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Разбор даты означает преобразование даты из текстового формата в объект даты, который программа может понимать и манипулировать. Это критически важно для чтения данных из различных источников, таких как пользовательский ввод или файлы, позволяя программам обрабатывать и управлять датами и временем последовательно.

## Как это сделать:
В Kotlin вы можете анализировать даты с помощью класса `LocalDateTime` из пакета `java.time`. Давайте разберем строку в дату.

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-01T15:30:00"
    val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    val parsedDate = LocalDateTime.parse(dateString, formatter)
    
    println(parsedDate)  // Пример вывода: 2023-04-01T15:30
}
```

## Подробнее
Kotlin не имеет собственной библиотеки даты и времени. Вместо этого он использует API `java.time`, представленный в Java 8, который заменил более старые, менее интуитивно понятные классы дат, такие как `java.util.Date`.

Большим плюсом `java.time` является то, что он принес неизменяемость и потокобезопасность в операции с датой-временем. До Java 8 за надежную работу с датами часто прибегали к сторонним библиотекам, таким как Joda-Time.

При анализе дат необходимо совпадение строки даты с правильным форматом. В противном случае вы столкнетесь с `DateTimeParseException`. Подход Kotlin основан на стандарте ISO 8601, но вы можете создавать собственные форматы с помощью `DateTimeFormatter` для разных шаблонов строк.

Альтернативы `LocalDateTime` включают `ZonedDateTime` для поддержки часовых поясов или `LocalDate` и `LocalTime` для раздельного анализа дат и времени. Гибкость Kotlin с API `java.time` гарантирует, что вы можете настроить разбор дат в соответствии с потребностями программы.

## Смотрите также
- Официальная документация Java `DateTimeFormatter`: [https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Документация Kotlin о взаимодействии с Java: [https://kotlinlang.org/docs/java-interop.html](https://kotlinlang.org/docs/java-interop.html)
- Форматы дат и времени ISO 8601: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
