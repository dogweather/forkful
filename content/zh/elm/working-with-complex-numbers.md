---
title:                "处理复数"
date:                  2024-01-26T04:39:36.734759-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"

category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么？
复数是实数和虚数的组合，像 `a + bi` 这样，其中 `i` 是 -1 的平方根。它们在工程学和物理学等领域中至关重要，用以解决常规数字无法触及的问题。

## 如何操作：
Elm 没有内置的复数支持，因此您需要创建自己的类型和函数。这里是一个快速设置：

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- 示例用法：
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum 是 { real = 4.0, imaginary = -2.0 }
```

## 深入探讨
从历史上看，复数并非总是被接受。它们在 16 世纪成为解决立方方程的游戏转换者。其他语言（例如 Python）提供内置的复数支持，可以直接进行操作。如你所见，Elm 需要一种 DIY 的方法。但您可以根据需要使其复杂化，构建乘法、除法和其他运算，解决性能问题。

## 另请参见
- Elm 的官方文档：https://package.elm-lang.org/ 用于创建自定义类型和掌握 Elm 基础。
- 数学史爱好者可以查看 Paul J. Nahin 的《An Imaginary Tale》一书，了解复数随时间的旅程。
- 深入数学导向的编程挑战，在 Project Euler (https://projecteuler.net) 应用您的复数魔法。
