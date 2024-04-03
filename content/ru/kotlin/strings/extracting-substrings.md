---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:25.748919-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Kotlin \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\
  \u0442\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u0438 `substring`, `take` \u0438\
  \ `drop`."
lastmod: '2024-03-13T22:44:44.957925-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Kotlin \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\
  \u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u0438 `substring`, `take` \u0438 `drop`."
title: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\u0434\
  \u0441\u0442\u0440\u043E\u043A"
weight: 6
---

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
