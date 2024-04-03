---
date: 2024-01-26 04:45:13.973662-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Ruby \u4F7F\u5F97\u5904\u7406\u590D\u6570\u53D8\
  \u5F97\u975E\u5E38\u7B80\u5355\u3002\u60A8\u53EF\u4EE5\u4F7F\u7528 Complex \u7C7B\
  \u6765\u521B\u5EFA\u548C\u64CD\u4F5C\u5B83\u4EEC\uFF1A."
lastmod: '2024-03-13T22:44:48.364435-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u4F7F\u5F97\u5904\u7406\u590D\u6570\u53D8\u5F97\u975E\u5E38\u7B80\u5355\
  \u3002\u60A8\u53EF\u4EE5\u4F7F\u7528 Complex \u7C7B\u6765\u521B\u5EFA\u548C\u64CD\
  \u4F5C\u5B83\u4EEC\uFF1A."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 如何操作:
Ruby 使得处理复数变得非常简单。您可以使用 Complex 类来创建和操作它们：

```ruby
require 'complex'

# 创建复数
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# 基本操作
sum = c1 + c2               # => (5.0+9.0i)
difference = c1 - c2        # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# 共轭、幅度和相位
conjugate = c1.conjugate    # => (3.0-4.0i)
magnitude = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 弧度

# 复数特有的方法
polar = c1.polar            # => [5.0, 0.9272952180016122]
rectangular = c1.rect       # => [3.0, 4.0]
```

## 深入了解
复数并不是新概念——它们自 16 世纪以来就存在，用于求解没有实数解的方程。除了数学，Ruby 的 Complex 类在计算方面做了大量工作，由 Math 模块支持以进行三角和超越函数运算。

早期的编程语言需要手动处理实部和虚部。一些语言，如 Fortran 和 C++，专门提供复数算术的特殊库。

Ruby 的方法将复数支持嵌入到其语法中，免除了您重新发明轮子的需要。在幕后，Complex类处理数学运算，而 Ruby 负责对象交互。

## 另见
- Ruby 关于 Complex 的文档: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorld 对复数的看法: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- 关于复数及其用途的视觉介绍: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
