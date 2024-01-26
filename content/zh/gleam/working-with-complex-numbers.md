---
title:                "处理复数"
date:                  2024-01-26T04:40:30.353163-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
复数具有实部和虚部（`a + bi`）。它们在电气工程和量子计算等多个领域非常有用。程序员使用它们来建模仅使用实数无法解决的方程。

## 如何操作：
Gleam不支持原生复数。你通常会自己实现或找到一个库。这里有一个快速示例，展示了如何实现基本操作：

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## 深入探讨

复数由Gerolamo Cardano在16世纪更正式地记录下来。它们是实数的自然扩展。然而，在像Gleam这样优先考虑性能和类型安全的年轻语言中，这样的特性是基本的（或者你自己动手做）。

在一些其他语言中，比如Python，复数是内置的（`3+4j`），使生活变得更容易。在Rust或Haskell中，你有库提供开箱即用的高级功能。

Gleam的方法意味着你必须处理所有方面：算术、极坐标、指数形式等。实现高效、准确的操作涉及仔细的编程，考虑浮点行为如何影响你的结果。

记得彻底测试，特别是边缘情况！如果你不小心，处理复数的无穷大和NaN（非数字）值可能会使你困惑。

## 另请参阅
想要更多好东西，这里是你可以深入探索的地方：
- [Gleam的官方文档](https://gleam.run/documentation/)
- 深入其他语言的库以获得灵感，如Rust的[num-complex](https://crates.io/crates/num-complex)或Python的[cmath模块](https://docs.python.org/3/library/cmath.html)。