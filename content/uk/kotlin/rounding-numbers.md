---
title:                "Округлення чисел"
aliases:
- uk/kotlin/rounding-numbers.md
date:                  2024-01-26T03:46:12.359856-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що та Чому?

Округлення чисел означає їх коригування до найближчого цілого числа або до певного ступеня точності. Програмісти роблять це, щоб покращити читабельність, зменшити вимоги до зберігання або тому, що точне значення не є критичним для подальших розрахунків.

## Як:

В Kotlin, округлення можна виконати за допомогою кількох функцій, таких як `roundToInt()`, `roundToDouble()`, та використовуючи `BigDecimal` для більшого контролю:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Виведення: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Виведення: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Виведення: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Виведення: 123.5
}
```

## Поглиблений Огляд

Історично, округлення чисел було фундаментальним концептом в математиці та обчисленнях, призначеним для вирішення обмежень числової точності. У ранній інформатиці, округлення було критично важливим через високу вартість пам'яті.

У Kotlin, округлення базується на стандартних Java бібліотеках. Опції для округлення включають `Math.round()`, яке округляє до найближчого цілого числа, та `BigDecimal` для налаштовуваного округлення, де ви можете вказати масштаб та `RoundingMode`.

Кожен `RoundingMode` має різні політики обробки ситуацій на межі (коли цифра точно посередині можливих варіантів для округлення). Наприклад, `RoundingMode.HALF_UP` округляє до найближчого сусіда, якщо обидва сусіди рівновіддалені, в цьому випадку він округляє вгору.

## Дивіться Також

- Документація Kotlin по [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Документація Java Oracle для [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- IEEE Стандарт для Арифметики з Плаваючою Крапкою (IEEE 754) [IEEE Стандарт 754](https://ieeexplore.ieee.org/document/4610935)
