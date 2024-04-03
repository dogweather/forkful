---
date: 2024-01-26 04:43:22.721495-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0414\
  \u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u0438\u0437\u043D\u0430\u0447\u0438\u043C\
  \u043E \u0431\u0430\u0437\u043E\u0432\u0438\u0439 \u043A\u043B\u0430\u0441 \u043A\
  \u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u043E\u0433\u043E \u0447\u0438\u0441\
  \u043B\u0430 \u0432 Kotlin."
lastmod: '2024-03-13T22:44:49.209058-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u0438\u0437\u043D\u0430\
  \u0447\u0438\u043C\u043E \u0431\u0430\u0437\u043E\u0432\u0438\u0439 \u043A\u043B\
  \u0430\u0441 \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u043E\u0433\u043E\
  \ \u0447\u0438\u0441\u043B\u0430 \u0432 Kotlin."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u0438\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

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
