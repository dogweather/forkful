---
title:                "数字取整"
aliases:
- zh/kotlin/rounding-numbers.md
date:                  2024-01-26T03:45:49.504117-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

四舍五入意味着将数字调整到最接近的整数或指定的精度等级。程序员这样做是为了提高可读性，减少存储需求，或因为对后续计算来说精确值并不关键。

## 如何操作：

在Kotlin中，可以使用几个函数进行四舍五入，如`roundToInt()`、`roundToDouble()`，并且使用`BigDecimal`进行更多控制：

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // 输出: 3

    val number2 = 3.5
    println(number2.roundToInt()) // 输出: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // 输出: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // 输出: 123.5
}
```

## 深入探讨

历史上，四舍五入一直是数学和计算中的一个基本概念，旨在处理数值精度限制。在早期计算机科学中，由于内存的高昂成本，四舍五入至关重要。

在Kotlin中，四舍五入基于标准Java库建立。四舍五入的选项包括`Math.round()`，它四舍五入到最接近的整数，以及`BigDecimal`用于可自定义的四舍五入，你可以指定一个规模和一个`RoundingMode`。

每个`RoundingMode`有处理平手（当数字恰好在四舍五入的两个选项中间时）的不同策略。例如，`RoundingMode.HALF_UP`四舍五入到最近的邻居，除非两个邻居都等距离，在这种情况下它四舍五入。

## 另见

- Kotlin文档中的[`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Oracle的Java文档中的[`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- IEEE浮点算术标准（IEEE 754）[IEEE标准754](https://ieeexplore.ieee.org/document/4610935)
