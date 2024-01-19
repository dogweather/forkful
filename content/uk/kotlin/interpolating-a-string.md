---
title:                "Інтерполяція рядка"
html_title:           "Kotlin: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?
Інтерполяція рядків - це техніка, яку використовують програмісти для створення рядків, які містять змінні значення. Наприклад, замість того, щоб ручно з'єднувати рядки та змінні, з інтерполяцією можна використовувати шаблони рядків для автоматичного підставляння значень змінних. Це зробить ваш код більш зрозумілим та зручним для редагування.

## Як це зробити:
```Kotlin
val name = "Mark"
val age = 25
println("Привіт, мене звати $name і мені $age років.") // виведе: Привіт, мене звати Mark і мені 25 років.
```
У цьому прикладі ми створили змінні з іменем та віком, а потім використали їх у рядку з використанням символу "$". Компілятор самостійно замінить символ на значення змінної, що дозволяє нам побачити повне речення.

## Глибинний занурення:
Історично інтерполяція рядків походить з мови программування Scala, але зараз підтримується багатьма іншими мовами, зокрема Kotlin. Іншим підходом до цієї задачі може бути використання регулярних виразів для пошуку та заміни значень у рядках.

## Також подивіться:
- [Офіційна документація Kotlin про інтерполяцію рядків](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- [Стаття на блозі про інтерполяцію рядків в Kotlin](https://blog.kotlin-academy.com/string-templates-in-kotlin-78f8e6d0667e)
- [Інші корисні прийоми програмування у Kotlin](https://kotlinlang.org/docs/reference/)