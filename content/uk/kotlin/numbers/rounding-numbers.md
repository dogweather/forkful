---
date: 2024-01-26 03:46:12.359856-07:00
description: "\u042F\u043A: \u0412 Kotlin, \u043E\u043A\u0440\u0443\u0433\u043B\u0435\
  \u043D\u043D\u044F \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u043D\
  \u0430\u0442\u0438 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\
  \u044E \u043A\u0456\u043B\u044C\u043A\u043E\u0445 \u0444\u0443\u043D\u043A\u0446\
  \u0456\u0439, \u0442\u0430\u043A\u0438\u0445 \u044F\u043A `roundToInt()`, `roundToDouble()`,\
  \ \u0442\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u044E\u0447\u0438 `BigDecimal` \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.210760-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Kotlin, \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F\
  \ \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u043D\u0430\u0442\u0438\
  \ \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043A\u0456\
  \u043B\u044C\u043A\u043E\u0445 \u0444\u0443\u043D\u043A\u0446\u0456\u0439, \u0442\
  \u0430\u043A\u0438\u0445 \u044F\u043A `roundToInt()`, `roundToDouble()`, \u0442\u0430\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\
  \u0438 `BigDecimal` \u0434\u043B\u044F \u0431\u0456\u043B\u044C\u0448\u043E\u0433\
  \u043E \u043A\u043E\u043D\u0442\u0440\u043E\u043B\u044E."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

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
