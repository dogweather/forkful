---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:08.063499-07:00
description: "\u041A\u0430\u043A: \u0412 Kotlin, \u043E\u043A\u0440\u0443\u0433\u043B\
  \u0435\u043D\u0438\u0435 \u043C\u043E\u0436\u043D\u043E \u0432\u044B\u043F\u043E\
  \u043B\u043D\u0438\u0442\u044C \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E\
  \ \u043D\u0435\u0441\u043A\u043E\u043B\u044C\u043A\u0438\u0445 \u0444\u0443\u043D\
  \u043A\u0446\u0438\u0439, \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `roundToInt()`,\
  \ `roundToDouble()`, \u0438 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u043D\u0438\u044F `BigDecimal` \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:44.968722-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Kotlin, \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435\
  \ \u043C\u043E\u0436\u043D\u043E \u0432\u044B\u043F\u043E\u043B\u043D\u0438\u0442\
  \u044C \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u043D\u0435\u0441\u043A\
  \u043E\u043B\u044C\u043A\u0438\u0445 \u0444\u0443\u043D\u043A\u0446\u0438\u0439\
  , \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `roundToInt()`, `roundToDouble()`,\
  \ \u0438 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\
  \u044F `BigDecimal` \u0434\u043B\u044F \u0431\u043E\u043B\u044C\u0448\u0435\u0433\
  \u043E \u043A\u043E\u043D\u0442\u0440\u043E\u043B\u044F."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Как:
В Kotlin, округление можно выполнить с помощью нескольких функций, таких как `roundToInt()`, `roundToDouble()`, и использования `BigDecimal` для большего контроля:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Выводит: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Выводит: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Выводит: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Выводит: 123.5
}
```

## Подробнее
Исторически, округление чисел было фундаментальной концепцией как в математике, так и в вычислениях, предназначенной для управления ограничениями числовой точности. В ранних компьютерах округление было критичным из-за высокой стоимости памяти.

В Kotlin, округление основано на стандартных библиотеках Java. Варианты округления включают `Math.round()`, которое округляет до ближайшего целого числа, и `BigDecimal` для настраиваемого округления, где вы можете указать масштаб и `RoundingMode`.

Каждый `RoundingMode` имеет разные политики для обработки равных значений (когда цифра находится точно посередине между вариантами округления). Например, `RoundingMode.HALF_UP` округляет до ближайшего соседа, если оба соседа находятся на равном расстоянии, в этом случае он округляет в большую сторону.

## Смотрите также
- Документация Kotlin о [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Документация Java Oracle о [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- Стандарт IEEE для арифметики с плавающей точкой (IEEE 754) [Стандарт IEEE 754](https://ieeexplore.ieee.org/document/4610935)
