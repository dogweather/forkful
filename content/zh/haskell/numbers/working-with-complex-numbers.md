---
date: 2024-01-26 04:42:05.246884-07:00
description: "\u590D\u6570\u7531\u4E00\u4E2A\u5B9E\u90E8\u548C\u4E00\u4E2A\u865A\u90E8\
  \u7EC4\u6210\uFF0C\u5728\u5DE5\u7A0B\u5B66\u3001\u7269\u7406\u5B66\u548C\u4FE1\u53F7\
  \u5904\u7406\u7B49\u5404\u79CD\u8BA1\u7B97\u9886\u57DF\u81F3\u5173\u91CD\u8981\u3002\
  \u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u4EEC\u6765\u89E3\u51B3\u5B9E\u6570\u65E0\u6CD5\
  \u89E3\u51B3\u7684\u65B9\u7A0B\uFF0C\u5982\u627E\u51FA\u8D1F\u6570\u7684\u6839\u3002"
lastmod: '2024-03-13T22:44:47.808565-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u7531\u4E00\u4E2A\u5B9E\u90E8\u548C\u4E00\u4E2A\u865A\u90E8\
  \u7EC4\u6210\uFF0C\u5728\u5DE5\u7A0B\u5B66\u3001\u7269\u7406\u5B66\u548C\u4FE1\u53F7\
  \u5904\u7406\u7B49\u5404\u79CD\u8BA1\u7B97\u9886\u57DF\u81F3\u5173\u91CD\u8981\u3002\
  \u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u4EEC\u6765\u89E3\u51B3\u5B9E\u6570\u65E0\u6CD5\
  \u89E3\u51B3\u7684\u65B9\u7A0B\uFF0C\u5982\u627E\u51FA\u8D1F\u6570\u7684\u6839\u3002"
title: "\u5904\u7406\u590D\u6570"
---

{{< edit_this_page >}}

## 什么及为什么？

复数由一个实部和一个虚部组成，在工程学、物理学和信号处理等各种计算领域至关重要。程序员使用它们来解决实数无法解决的方程，如找出负数的根。

## 如何操作：

Haskell通过`Data.Complex`模块来处理复数。这里是一个快速指南：

```haskell
import Data.Complex

-- 定义两个复数
let z1 = 3 :+ 4  -- 即 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- 算术操作
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- 复共轭
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- 幅度和相位
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- 极坐标到直角坐标转换，反之亦然
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- 与z1相同
```

在GHCi中加载上述代码后的示例输出可能是：

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## 深入探索

复数可以追溯到16世纪，但直到 much later 广泛接受。Haskell和许多其他语言一样，为复数算术提供了原生支持，使得无需实现底层数学就可以轻松地使用这些数字。

替代方案包括构建自定义的复数类型或使用特定域的库，如用于3D图形的四元数。但对于大多数用例而言，Haskell的`Data.Complex`已经足够。

在底层，`Data.Complex`只是一种将两个`Float`或`Double`值配对的数据类型，分别代表实部和虚部。这是在Haskell平台上使用复数的一种简单且有效的方式。

## 参见

请查看这些资源以了解更多关于Haskell中复数的信息：

- 官方Haskell `Data.Complex`文档：[Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- 更深入地了解Haskell的数字类型：[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- 探索Haskell中的快速傅立叶变换算法：[Haskell FFT库](https://hackage.haskell.org/package/fft)
