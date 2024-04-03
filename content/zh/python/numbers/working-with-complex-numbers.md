---
date: 2024-01-26 04:44:54.740864-07:00
description: "\u590D\u6570\u662F\u5F62\u5F0F\u4E3A `a + bi` \u7684\u4E00\u7EC4\u6570\
  \u5B57\uFF0C\u5176\u4E2D `a` \u548C `b` \u662F\u5B9E\u6570\uFF0C`i` \u662F\u865A\
  \u6570\u5355\u4F4D\uFF08`i^2 = -1`\uFF09\u3002\u5728\u7F16\u7A0B\u4E2D\uFF0C\u6211\
  \u4EEC\u4F7F\u7528\u5B83\u4EEC\u6765\u89E3\u51B3\u5404\u79CD\u9886\u57DF\u7684\u95EE\
  \u9898\uFF0C\u6BD4\u5982\u7535\u5B50\u5DE5\u7A0B\u3001\u4FE1\u53F7\u5904\u7406\u548C\
  \u91CF\u5B50\u8BA1\u7B97\u3002"
lastmod: '2024-03-13T22:44:47.248336-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u662F\u5F62\u5F0F\u4E3A `a + bi` \u7684\u4E00\u7EC4\u6570\u5B57\
  \uFF0C\u5176\u4E2D `a` \u548C `b` \u662F\u5B9E\u6570\uFF0C`i` \u662F\u865A\u6570\
  \u5355\u4F4D\uFF08`i^2 = -1`\uFF09\u3002\u5728\u7F16\u7A0B\u4E2D\uFF0C\u6211\u4EEC\
  \u4F7F\u7528\u5B83\u4EEC\u6765\u89E3\u51B3\u5404\u79CD\u9886\u57DF\u7684\u95EE\u9898\
  \uFF0C\u6BD4\u5982\u7535\u5B50\u5DE5\u7A0B\u3001\u4FE1\u53F7\u5904\u7406\u548C\u91CF\
  \u5B50\u8BA1\u7B97\u3002."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 什么 & 为什么？
复数是形式为 `a + bi` 的一组数字，其中 `a` 和 `b` 是实数，`i` 是虚数单位（`i^2 = -1`）。在编程中，我们使用它们来解决各种领域的问题，比如电子工程、信号处理和量子计算。

## 如何操作：
Python 对复数有内置支持。以下是你可以操作它们的方法：

```Python
# 创建复数
z = 4 + 5j
print(z)  # 输出：(4+5j)

# 访问实部和虚部
print(z.real)  # 输出：4.0
print(z.imag)  # 输出：5.0

# 复数运算
w = 1 - 2j
print(z + w)  # 输出：(5+3j)
print(z - w)  # 输出：(3+7j)
print(z * w)  # 输出：(14+2j)
print(z / w)  # 输出：(-3.6+1.2j)

# 模（绝对值）
print(abs(z))  # 输出：6.4031242374328485

# 复数的共轭
print(z.conjugate())  # 输出：(4-5j)
```

## 深入探讨
复数最初由16世纪的杰罗拉莫·卡尔达诺提出。Python，与其他编程语言一样，将复数视为一等公民。这意味着它们内置于语言中，具有易于使用的功能，避免了为基本操作导入外部库的需求。

然而，对于重度数值计算，Python 有一个名为 `cmath` 的库，专门用于复数。它包含了诸如 `exp`、`log` 和三角运算等额外功能。

当 Python 不足以满足需求时，你可能会转向像 NumPy 这样的库，特别是对涉及复数的数组操作。NumPy 提供了对数值计算性能至关重要的优化和矢量化操作。

## 参考资料
查看以下资源以了解更多信息：

- Python 官方文档关于复数的部分：https://docs.python.org/3/library/stdtypes.html#typesnumeric
- `cmath` 模块文档：https://docs.python.org/3/library/cmath.html
- 处理复数数组的 NumPy：https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
