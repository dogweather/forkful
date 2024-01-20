---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і чому?

Видалення символів, що відповідають паттерну, це процес видалення конкретного набору символів з рядка в програмуванні. Це допомагає програмістам очистити дані або маніпулювати рядками за потреби.

## Як це зробити:

```Kotlin
fun main() {
    val pattern = Regex("[aeiou]")  // Співпадання будь-якої голосної
    val input = "Hello, World!"
    
    val output = pattern.replace(input, "")
    println(output)  // Виведе: "Hll, Wrld!"
}
```
Вищенаведений код видаляє всі голосні з рядка "Hello, World!".

## Поглиблене вивчення:

1. Історичний контекст: Процес видалення символів, що відповідають паттерну, існує стільки, скільки й самі початки програмування. Ця операція зазвичай використовується для очищення вхідних даних або підготовки рядків до подальшого оброблення.

2. Альтернативи: Ви також можете використовувати `filterNot` в Kotlin для видалення символів, що відповідають паттерну.

   ```Kotlin
    fun main() {
       val input = "Hello, World!"
       val output = input.filterNot { it in 'a'..'z' || it in 'A'..'Z' }  // Видаляємо все, що є буквою.
       println(output)  // Виведе: ", !"
    }
   ```
   
3. Реалізація: Коли ви використовуєте `Regex(pattern).replace(input, "")` в Kotlin, Ви використовуєте вбудований об'єкт `Regex` для створення паттерну, а потім застосовуєте його до вхідного рядка. Зазначена заміна порожнім рядком ("") ефективно видаляє співпадання переданого паттерну.

## Див. також:

1. [Kotlin API документація: Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
2. [Kotlin API документація: String.filterNot](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/filter-not.html)
3. [Туторіал: Робота з рядками в Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/strings.html)