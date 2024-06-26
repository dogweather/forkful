---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:52.510647-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043E\
  \u043F\u0440\u0435\u0434\u0435\u043B\u0438\u043C \u0431\u0430\u0437\u043E\u0432\u044B\
  \u0439 \u043A\u043B\u0430\u0441\u0441 \u043A\u043E\u043C\u043F\u043B\u0435\u043A\
  \u0441\u043D\u043E\u0433\u043E \u0447\u0438\u0441\u043B\u0430 \u043D\u0430 Kotlin."
lastmod: '2024-03-13T22:44:44.966837-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043E\u043F\u0440\u0435\u0434\
  \u0435\u043B\u0438\u043C \u0431\u0430\u0437\u043E\u0432\u044B\u0439 \u043A\u043B\
  \u0430\u0441\u0441 \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u043E\u0433\
  \u043E \u0447\u0438\u0441\u043B\u0430 \u043D\u0430 Kotlin."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u044B\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

## Как:
Давайте определим базовый класс комплексного числа на Kotlin:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // Вывод: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Вывод: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Вывод: a * b = (-5.0 + 10.0i)
}
```

## Углубленно
Комплексные числа впервые были упомянуты в 16 веке, решая кубические уравнения, которые не имели действительных решений. Инженерия и физика в значительной степени выигрывают от использования комплексных чисел для анализа цепей переменного тока и волновых форм. В качестве альтернативы, для серьезной работы, вы могли бы использовать библиотеку, такую как `koma` или `ejml` от Kotlin.

Операции с комплексными числами зеркально повторяют операции с действительными числами, но с учетом мнимой единицы. Умножение, например, следует закону распределения, помня, что `i^2 = -1`. Эта мнимая единица позволяет нам представлять многомерные числа, критически важные в различных научных вычислениях.

## Смотрите также
Библиотеки математики на Kotlin:

- [koma](https://koma.kyonifer.com/): Научно-вычислительная библиотека для Kotlin.

Дополнительное чтение о комплексных числах:

- [Википедия: Комплексные числа](https://en.wikipedia.org/wiki/Complex_number)
