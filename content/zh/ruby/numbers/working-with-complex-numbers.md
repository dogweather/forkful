---
date: 2024-01-26 04:45:13.973662-07:00
description: "\u590D\u6570\uFF0C\u7531\u4E00\u4E2A\u5B9E\u90E8\u548C\u4E00\u4E2A\u865A\
  \u90E8\u7EC4\u6210\uFF08\u5982 3+4i\uFF09\uFF0C\u5728\u5DE5\u7A0B\u548C\u7269\u7406\
  \u5B66\u4E2D\u662F\u57FA\u672C\u6982\u5FF5\u3002\u7A0B\u5E8F\u5458\u5728\u6A21\u62DF\
  \u3001\u4FE1\u53F7\u5904\u7406\u548C\u89E3\u51B3\u4EC5\u4F7F\u7528\u5B9E\u6570\u884C\
  \u4E0D\u901A\u7684\u65B9\u7A0B\u65F6\u4F1A\u7528\u5230\u590D\u6570\u3002"
lastmod: '2024-03-13T22:44:48.364435-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\uFF0C\u7531\u4E00\u4E2A\u5B9E\u90E8\u548C\u4E00\u4E2A\u865A\
  \u90E8\u7EC4\u6210\uFF08\u5982 3+4i\uFF09\uFF0C\u5728\u5DE5\u7A0B\u548C\u7269\u7406\
  \u5B66\u4E2D\u662F\u57FA\u672C\u6982\u5FF5\u3002\u7A0B\u5E8F\u5458\u5728\u6A21\u62DF\
  \u3001\u4FE1\u53F7\u5904\u7406\u548C\u89E3\u51B3\u4EC5\u4F7F\u7528\u5B9E\u6570\u884C\
  \u4E0D\u901A\u7684\u65B9\u7A0B\u65F6\u4F1A\u7528\u5230\u590D\u6570\u3002"
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 什么 & 为什么?
复数，由一个实部和一个虚部组成（如 3+4i），在工程和物理学中是基本概念。程序员在模拟、信号处理和解决仅使用实数行不通的方程时会用到复数。

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
