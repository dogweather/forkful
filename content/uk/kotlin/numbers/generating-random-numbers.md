---
title:                "Генерація випадкових чисел"
date:                  2024-01-27T20:34:40.087045-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерація випадкових чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?

Генерація випадкових чисел у програмуванні полягає у створенні чисел, які не мають жодного прогнозованого візерунку. Програмісти роблять це з різних причин, у тому числі для симуляцій, тестування алгоритмів, ігор та застосунків безпеки, де непередбачуваність є ключовою для досягнення реалістичних або безпечних результатів.

## Як це зробити:

Kotlin надає простий спосіб генерації випадкових чисел через свою стандартну бібліотеку. Ось як ви можете генерувати різні типи випадкових значень:

### Генерація випадкового цілого числа

Щоб згенерувати випадкове ціле число в певному діапазоні:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Генерує випадкове число від 1 до 99
    println(randomNumber)
}
```

### Генерація випадкового дробового числа

Аналогічно, для генерації випадкового дробового числа:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Генерує випадкове дробове число від 1.0 до 10.0
    println(randomDouble)
}
```

### Генерація випадкового булевого значення

Для генерації випадкового булевого значення:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Випадково генерує true або false
    println(randomBoolean)
}
```

### Встановлення початкового значення для відтворюваних результатів

У випадках, коли вам потрібні відтворювані послідовності випадкових чисел (наприклад, при тестуванні), ви можете встановити початкове значення генератора випадкових чисел:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Поглиблений аналіз

Підхід стандартної бібліотеки Kotlin до генерації випадкових чисел використовує `java.util.Random`, що забезпечує поєднання простоти використання та продуктивності. Однак важливо зазначити, що ці методи генерують псевдовипадкові числа, що означає, що числа здаються випадковими, але генеруються за допомогою детермінованого процесу.

Для більшості застосунків випадковість, надана класом `Random` Kotlin, є достатньою. Однак для застосунків, чутливих до безпеки, таких як криптографія, де якість випадковості є найважливішою, слід розглянути використання `java.security.SecureRandom` замість цього. SecureRandom спеціально призначений для криптографічних операцій, забезпечуючи вищу якість випадковості, хоча з потенційним зниженням продуктивності.

Kotlin не винаходить колесо заново, але пропонує Kotlin-орієнтоване API над механізмами генерації випадкових чисел Java, роблячи його більш ідіоматичним та лаконічним для використання у проектах Kotlin. Як завжди, коли мова йде про випадковість, програмісти повинні ретельно враховувати конкретний випадок використання, щоб вибрати найбільш відповідний інструмент для роботи.