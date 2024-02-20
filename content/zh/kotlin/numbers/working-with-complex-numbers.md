---
date: 2024-01-26 04:43:13.584784-07:00
description: "\u590D\u6570\u6269\u5C55\u4E86\u6211\u4EEC\u7684\u6570\u5B57\u7CFB\u7EDF\
  \uFF0C\u5305\u62EC\u8D1F\u6570\u7684\u5E73\u65B9\u6839\uFF0C\u5728\u8FD9\u91CC\u201C\
  \u865A\u6570\u201D\u5355\u5143i\u7B49\u4E8E-1\u7684\u5E73\u65B9\u6839\u3002\u7A0B\
  \u5E8F\u5458\u5728\u5DE5\u7A0B\u5B66\u3001\u7269\u7406\u5B66\u548C\u4FE1\u53F7\u5904\
  \u7406\u7B49\u9886\u57DF\u4F7F\u7528\u5B83\u4EEC\uFF0C\u56E0\u4E3A\u5B83\u4EEC\u975E\
  \u5E38\u9002\u5408\u6A21\u62DF\u6CE2\u52A8\u3001\u632F\u8361\u4EE5\u53CA\u4EFB\u4F55\
  \u65CB\u8F6C\u7684\u4E1C\u897F\u3002"
lastmod: 2024-02-19 22:05:06.742998
model: gpt-4-0125-preview
summary: "\u590D\u6570\u6269\u5C55\u4E86\u6211\u4EEC\u7684\u6570\u5B57\u7CFB\u7EDF\
  \uFF0C\u5305\u62EC\u8D1F\u6570\u7684\u5E73\u65B9\u6839\uFF0C\u5728\u8FD9\u91CC\u201C\
  \u865A\u6570\u201D\u5355\u5143i\u7B49\u4E8E-1\u7684\u5E73\u65B9\u6839\u3002\u7A0B\
  \u5E8F\u5458\u5728\u5DE5\u7A0B\u5B66\u3001\u7269\u7406\u5B66\u548C\u4FE1\u53F7\u5904\
  \u7406\u7B49\u9886\u57DF\u4F7F\u7528\u5B83\u4EEC\uFF0C\u56E0\u4E3A\u5B83\u4EEC\u975E\
  \u5E38\u9002\u5408\u6A21\u62DF\u6CE2\u52A8\u3001\u632F\u8361\u4EE5\u53CA\u4EFB\u4F55\
  \u65CB\u8F6C\u7684\u4E1C\u897F\u3002"
title: "\u5904\u7406\u590D\u6570"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
复数扩展了我们的数字系统，包括负数的平方根，在这里“虚数”单元i等于-1的平方根。程序员在工程学、物理学和信号处理等领域使用它们，因为它们非常适合模拟波动、振荡以及任何旋转的东西。

## 如何操作：

让我们在Kotlin中定义一个基本的复数类：

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
    
    println("a + b = ${a + b}")  // 输出: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // 输出: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // 输出: a * b = (-5.0 + 10.0i)
}
```

## 深入了解

复数最早在16世纪被提及，用于解决缺乏实数解的三次方程。在分析交流电路和波形方面，复数给工程学和物理学带来了极大的好处。你也可以选择使用Kotlin的`koma`或`ejml`库来进行繁重的工作。

复数上的运算反映了实数的运算，但需要注意虚数单位。例如，乘法遵循分配律，记住`i^2 = -1`。这个虚数单位使我们能够表示多维数字，在各种科学计算中至关重要。

## 另请参阅

Kotlin数学库：

- [koma](https://koma.kyonifer.com/)：一个用于Kotlin的科学计算库。

关于复数的进一步阅读：

- [Wikipedia: 复数](https://en.wikipedia.org/wiki/Complex_number)
