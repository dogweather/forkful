---
title:                "Извлечение подстрок"
date:                  2024-01-28T23:57:25.748919-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Извлечение подстрок означает выборку конкретных частей из строки. Мы делаем это для того, чтобы манипулировать или анализировать текстовые данные, например, извлекая имена пользователей из адресов электронной почты или выделая даты для получения месяца.

## Как это сделать:
В Kotlin используйте функции `substring`, `take` и `drop`.

```Kotlin
fun main() {
    val text = "Привет, Kotlin!"

    println(text.substring(7, 13)) // Печатает "Kotlin"
    
    // С начала
    println(text.take(6)) // Печатает "Привет"

    // С конца
    println(text.takeLast(6)) // Печатает "Kotlin!"

    // Удаление символов
    println(text.drop(7)) // Печатает "Kotlin!"
}
```

## Подробнее
В ранние дни программирования обработка строк была ручной и подверженной ошибкам. В Kotlin это проще, безопаснее и менее ресурсоемко благодаря встроенным функциям и возможностям класса String.

Альтернативы `substring` включают использование регулярных выражений с `Regex` или `split` для разделения строк, но эти методы могут быть излишними для простых задач.

С точки зрения реализации, помните, что строки в Kotlin неизменяемы. Таким образом, когда вы извлекаете подстроку, вы на самом деле создаете новый объект String, а не изменяете оригинальный.

## Смотрите также
- Документация по строкам Kotlin: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Регулярные выражения в Kotlin для продвинутой манипуляции с текстом: [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
