---
title:                "Видалення символів за візерунком"
aliases:
- uk/kotlin/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:44.683640-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Видалення символів, що відповідають паттерну, полягає в тому, щоб очистити рядок від непотрібних або небезпечних знаків. Програмісти роблять це для валідації вводу, безпеки даних і оптимізації інформації.

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
