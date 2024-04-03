---
date: 2024-01-20 17:42:44.683640-07:00
description: "How to: (\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) ."
lastmod: '2024-03-13T22:44:49.193444-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

## How to: (Як зробити:)
```kotlin
fun main() {
    val inputString = "Програмування в Kotlin - це круто!123"
    val pattern = "\\d+".toRegex() // Паттерн для видалення усіх цифр
    val resultString = inputString.replace(pattern, "")
    println(resultString)
}
```

Output:
```
Програмування в Kotlin - це круто!
```

## Deep Dive (Занурення у Деталі)
Видалення символів за паттерном в Kotlin базується на регулярних виразах, інструменті з історією, що йде від 1950-х. Альтернативи регулярним виразам включають роботу зі строками через `filter` або використання `forEach` для перебору символів. При видаленні великої кількості даних ефективність може варіюватись, тому варто обирати підхід залежно від ситуації.

## See Also (Дивіться Також)
- [Kotlin Documentation on Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Kotlin `replace` function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
