---
title:                "Робота з комплексними числами"
aliases:
- uk/kotlin/working-with-complex-numbers.md
date:                  2024-01-26T04:43:22.721495-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Що та чому?
Комплексні числа розширюють нашу числову систему, включаючи корені від’ємних чисел, де "уявна" одиниця i дорівнює кореню з -1. Програмісти використовують їх у таких галузях, як інженерія, фізика та обробка сигналів, оскільки вони чудово моделюють хвилі, коливання та все, що обертається.

## Як це робити:

Давайте визначимо базовий клас комплексного числа в Kotlin:

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
    
    println("a + b = ${a + b}")  // Вивід: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Вивід: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Вивід: a * b = (-5.0 + 10.0i)
}
```

## Поглиблений огляд

Комплексні числа вперше були згадані у 16 столітті при розв'язанні кубічних рівнянь, що не мали дійсних рішень. Інженерія та фізика значною мірою виграють від комплексних чисел для аналізу змінних струмів та хвиль. Для складніших розрахунків ви також можете використовувати бібліотеки, як-от `koma` чи `ejml` від Kotlin.

Операції над комплексними числами повторюють операції з дійсними числами, але зі зверненням уваги на уявну одиницю. Множення, наприклад, слідує за правилом дистрибутивності, пам'ятаючи, що `i^2 = -1`. Ця уявна одиниця дає нам можливість представляти багатовимірні числа, які є ключовими у різноманітних наукових обчисленнях.

## Дивіться також

Бібліотеки Kotlin Math:

- [koma](https://koma.kyonifer.com/): Бібліотека наукових обчислень для Kotlin.

Додаткове читання про комплексні числа:

- [Wikipedia: Комплексні числа](https://en.wikipedia.org/wiki/Complex_number)
