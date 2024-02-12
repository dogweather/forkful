---
title:                "Преобразование даты в строку"
aliases:
- /ru/kotlin/converting-a-date-into-a-string/
date:                  2024-01-28T23:56:42.374732-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование даты в строку"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Конвертирование даты в строку означает представление определенного момента в читаемом для человека формате. Программисты делают это, чтобы отображать даты пользователям или сериализовывать их для хранения и передачи данных.

## Как:
В Kotlin вы можете преобразовать `Date` в `String` с использованием класса `SimpleDateFormat`. Давайте напишем немного кода:

```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val date = Date() // Создаем объект Date для текущего времени
    val format = SimpleDateFormat("yyyy-MM-dd HH:mm:ss") // Определяем шаблон даты
    val dateString = format.format(date) // Преобразуем Date в String
    println(dateString) // Выводим строку с датой
}
```

Пример вывода может выглядеть так:

```
2023-03-25 14:45:32
```

## Погружение в детали
До появления `java.time` класс `SimpleDateFormat` был основным инструментом для преобразования даты в строку в Java и, по наследству, в Kotlin. Да, Kotlin работает на Java Virtual Machine и удобно взаимодействует с библиотеками Java.

Однако с появлением Java 8 `java.time` вошел в игру, привнеся `DateTimeFormatter` с гораздо более совершенным API. Это стало переломным моментом, предложив безопасное, неизменяемое и потокобезопасное управление датой и временем. Поддержка этого в Kotlin реализована без проблем:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDate = LocalDateTime.now() // Получаем текущую дату и время
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formattedDate = currentDate.format(formatter)
    println(formattedDate)
}
```

Альтернативы? Конечно. Для нестандартных требований или при работе с несколькими библиотеками дат, сторонние опции вроде Joda-Time ранее считались золотым стандартом. На сегодняшний день `java.time` покрывает большинство потребностей.

Что касается деталей реализации, `SimpleDateFormat` не является потокобезопасным, что означает, что он может споткнуться, когда используется в параллельных настройках. У `DateTimeFormatter` такой проблемы нет. Создайте один раз, используйте вечно — или, по крайней мере, на протяжении всего вашего приложения, не беспокоясь особо.

## См. также
- `DateTimeFormatter` JavaDoc для всех ваших потребностей в шаблонах: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Если вы чувствуете ностальгию или нужны примеры для устаревших систем, вот информация о `SimpleDateFormat`: [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
