---
title:                "Работа с комплексными числами"
aliases:
- ru/kotlin/working-with-complex-numbers.md
date:                  2024-01-29T00:05:52.510647-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с комплексными числами"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/working-with-complex-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Комплексные числа расширяют нашу числовую систему за счет включения квадратных корней отрицательных чисел, где 'мнимая' единица i равна квадратному корню из -1. Программисты используют их в таких областях, как инженерия, физика и обработка сигналов, потому что они отлично подходят для моделирования волн, колебаний и всего, что вращается.

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
