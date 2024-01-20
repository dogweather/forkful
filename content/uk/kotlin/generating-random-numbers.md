---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Генерація випадкових чисел - це процес створення чисел, що не залежать один від одного. Програмісти часто використовують це для створення різноманітного контенту або моделювання випадкових подій.

## Як робити:
В Kotlin ви можете генерувати випадкові числа так, як це показано нижче.

```Kotlin
import kotlin.random.Random

fun main() {
   val randomNumber = Random.nextInt(1, 100)
   println(randomNumber)
}
```

Цей код виведе число від 1 до 99.

## Детальніше:
Генерація випадкових чисел використовується з часів початку комп’ютерної ери. У деяких випадках ви можете захотіти використати secure random (спеціальний клас, який генерує більш безпечні випадкові числа). Також варто враховувати, що клас `Random` в Kotlin генерує псевдовипадкові числа, які залежать від так званого "seed" (початкового числа).

```Kotlin
import kotlin.random.Random

fun main() {
    val random = Random(123)
    print(random.nextInt())
}
```

У цьому прикладі буде генеруватися одне й те ж число кожного разу, коли ви запустите програму.

## Дивіться також:
* [Докладніше про Random в Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
* [Ефективні методи генерації випадкових чисел](https://sample-space.com/uk/efficient-methods-for-random-number-generation/)