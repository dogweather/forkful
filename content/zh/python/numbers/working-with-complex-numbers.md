---
date: 2024-01-26 04:44:54.740864-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u590D\u6570\u6700\u521D\u753116\u4E16\
  \u7EAA\u7684\u6770\u7F57\u62C9\u83AB\xB7\u5361\u5C14\u8FBE\u8BFA\u63D0\u51FA\u3002\
  Python\uFF0C\u4E0E\u5176\u4ED6\u7F16\u7A0B\u8BED\u8A00\u4E00\u6837\uFF0C\u5C06\u590D\
  \u6570\u89C6\u4E3A\u4E00\u7B49\u516C\u6C11\u3002\u8FD9\u610F\u5473\u7740\u5B83\u4EEC\
  \u5185\u7F6E\u4E8E\u8BED\u8A00\u4E2D\uFF0C\u5177\u6709\u6613\u4E8E\u4F7F\u7528\u7684\
  \u529F\u80FD\uFF0C\u907F\u514D\u4E86\u4E3A\u57FA\u672C\u64CD\u4F5C\u5BFC\u5165\u5916\
  \u90E8\u5E93\u7684\u9700\u6C42\u3002 \u7136\u800C\uFF0C\u5BF9\u4E8E\u91CD\u5EA6\u6570\
  \u503C\u8BA1\u7B97\uFF0CPython \u6709\u4E00\u4E2A\u540D\u4E3A `cmath` \u7684\u5E93\
  \uFF0C\u4E13\u95E8\u7528\u4E8E\u590D\u6570\u3002\u5B83\u5305\u542B\u4E86\u8BF8\u5982\
  \u2026"
lastmod: '2024-04-05T22:51:00.476198-06:00'
model: gpt-4-0125-preview
summary: "\u7136\u800C\uFF0C\u5BF9\u4E8E\u91CD\u5EA6\u6570\u503C\u8BA1\u7B97\uFF0C\
  Python \u6709\u4E00\u4E2A\u540D\u4E3A `cmath` \u7684\u5E93\uFF0C\u4E13\u95E8\u7528\
  \u4E8E\u590D\u6570\u3002\u5B83\u5305\u542B\u4E86\u8BF8\u5982 `exp`\u3001`log` \u548C\
  \u4E09\u89D2\u8FD0\u7B97\u7B49\u989D\u5916\u529F\u80FD\u3002"
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

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
