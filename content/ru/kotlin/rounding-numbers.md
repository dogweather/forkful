---
title:                "Округление чисел"
date:                  2024-01-29T00:02:08.063499-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округление чисел"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/rounding-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Округление чисел означает их корректировку до ближайшего целого числа или до определенной степени точности. Программисты делают это для повышения читаемости, сокращения требований к хранению или потому что точное значение не критично для последующих расчетов.

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
