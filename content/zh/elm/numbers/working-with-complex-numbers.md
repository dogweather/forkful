---
date: 2024-01-26 04:39:36.734759-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elm \u6CA1\u6709\u5185\u7F6E\u7684\u590D\
  \u6570\u652F\u6301\uFF0C\u56E0\u6B64\u60A8\u9700\u8981\u521B\u5EFA\u81EA\u5DF1\u7684\
  \u7C7B\u578B\u548C\u51FD\u6570\u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\u5FEB\u901F\u8BBE\
  \u7F6E\uFF1A."
lastmod: '2024-04-05T21:53:47.983479-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

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
