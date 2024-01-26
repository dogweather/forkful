---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:57.673812-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Генерування випадкових чисел – це створення непередбачуваних чисел за допомогою коду. Програмісти використовують випадкові числа для ігор, симуляцій, тестування алгоритмів, криптографії та інше.

## Як це зробити:
```kotlin
import kotlin.random.Random

fun main() {
    // Single random integer between 0 (inclusive) and 100 (exclusive)
    val randomNumber = Random.nextInt(0, 100)
    println(randomNumber)

    // Generate a random double
    val randomDouble = Random.nextDouble()
    println(randomDouble)

    // Generate a list of random integers
    val randomInts = List(5) { Random.nextInt(0, 100) }
    println(randomInts)
}
```
Вивід прикладу (результат буде відрізнятися при кожному запуску):
```
42
0.8124020724
[11, 58, 34, 90, 22]
```

## Поглиблений розгляд:
Раніше, до появи комп’ютерів, випадкові числа генерували фізично - за допомогою кубиків чи лотерейних барабанів. В комп'ютерних системах використовують псевдовипадкові числа, оскільки справжнє випадкове генерування числа на детермінованій машині неможливе. Алгоритми, які використовуються для цього, створюють послідовності чисел, які виглядають випадковими, але насправді є передбачуваними, якщо знати вихідний стан (seed). Для більш випадкових чисел можна використовувати `SecureRandom`, який створює числа на основі більш непередбачуваних даних, наприклад, коливаннях вільного часу процесора.

## Дивіться також:
- Kotlin Official Documentation: [Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- Oracle Java Documentation: [SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- Towards Data Science: Understanding randomness and random number generators in simulations [Understanding Randomness](https://towardsdatascience.com/understanding-randomness-3b3e6b422cd)
