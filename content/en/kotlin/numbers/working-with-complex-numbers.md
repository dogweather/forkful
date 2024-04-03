---
date: 2024-01-25 02:59:46.766554-07:00
description: 'How to: Let''s define a basic complex number class in Kotlin.'
lastmod: '2024-03-13T22:45:00.042953-06:00'
model: gpt-4-1106-preview
summary: Let's define a basic complex number class in Kotlin.
title: Working with complex numbers
weight: 14
---

## How to:
Let's define a basic complex number class in Kotlin:

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
    
    println("a + b = ${a + b}")  // Output: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Output: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Output: a * b = (-5.0 + 10.0i)
}
```

## Deep Dive
Complex numbers were first mentioned in the 16th century, solving cubic equations that lacked real solutions. Engineering and physics vastly benefit from complex numbers for analyzing AC circuits and waveforms. You could alternatively use a library like Kotlin's `koma` or `ejml` for heavy-duty work.

Operations on complex numbers mirror real numbers, but with attention to the imaginary unit. Multiplication, for instance, follows the distributive property, remembering that `i^2 = -1`. This imaginary unit enables us to represent multi-dimensional numbers, crucial in various scientific computations.

## See Also
Kotlin Math libraries:

- [koma](https://koma.kyonifer.com/): A scientific computing library for Kotlin.

Further reading on Complex Numbers:

- [Wikipedia: Complex Numbers](https://en.wikipedia.org/wiki/Complex_number)
