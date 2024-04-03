---
date: 2024-01-26 03:36:59.087763-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5047\u8BBE\u4F60\u6709\u4E00\u6BB5\u4EE3\
  \u7801\uFF0C\u5B83\u80FD\u6839\u636E\u957F\u548C\u5BBD\u8BA1\u7B97\u5E76\u6253\u5370\
  \u51FA\u77E9\u5F62\u7684\u9762\u79EF\u548C\u5468\u957F\u3002\u5B83\u80FD\u5B8C\u6210\
  \u5DE5\u4F5C\uFF0C\u4F46\u662F\u91CD\u590D\u4E14\u6709\u70B9\u4E71\u3002"
lastmod: '2024-03-13T22:44:47.265192-06:00'
model: gpt-4-0125-preview
summary: "\u5047\u8BBE\u4F60\u6709\u4E00\u6BB5\u4EE3\u7801\uFF0C\u5B83\u80FD\u6839\
  \u636E\u957F\u548C\u5BBD\u8BA1\u7B97\u5E76\u6253\u5370\u51FA\u77E9\u5F62\u7684\u9762\
  \u79EF\u548C\u5468\u957F\u3002\u5B83\u80FD\u5B8C\u6210\u5DE5\u4F5C\uFF0C\u4F46\u662F\
  \u91CD\u590D\u4E14\u6709\u70B9\u4E71."
title: "\u91CD\u6784"
weight: 19
---

## 如何操作：
假设你有一段代码，它能根据长和宽计算并打印出矩形的面积和周长。它能完成工作，但是重复且有点乱。

```python
# 原始版本
length = 4
width = 3

# 计算面积和周长
area = length * width
perimeter = 2 * (length + width)

print("面积:", area)
print("周长:", perimeter)
```

我们可以通过将功能封装进函数来重构这段代码，这使得代码更加有组织和可复用：

```python
# 重构版本

def calculate_area(length, width):
    return length * width

def calculate_perimeter(length, width):
    return 2 * (length + width)

# 使用
length = 4
width = 3

print("面积:", calculate_area(length, width))
print("周长:", calculate_perimeter(length, width))
```

两个代码片段输出相同的结果：
```
面积: 12
周长: 14
```

但重构后的版本更简洁，并且分离了关注点，这使得更新一个计算而不影响另一个变得更容易。

## 深入探索
重构的根源在于软件工程的早期，当时程序员意识到即使代码已经“能工作”，代码也可以——而且应该——被改进。Martin Fowler 的开创性著作《重构：改善既有代码的设计》阐述了许多核心原则和技术。他著名地说：“任何傻瓜都能写出计算机能理解的代码。好的程序员编写人类能理解的代码。”

重构的替代方案可能包括从头开始重写代码或在没有系统性改进的情况下进行小的调整。然而，与重写相比，重构通常更具成本效益，且与临时修改相比风险更小。实现细节可能特定于每种编程范型；然而，面向对象编程特别适合进行重构，特别是使用诸如提取方法（就像我们的`calculate_area`和`calculate_perimeter`函数）、内联、在对象间移动特性以及重命名方法或变量以提高清晰度的技巧。

在 Python 中进行重构常使用诸如 `PyCharm` 这样内建重构能力的工具，或 `rope`，一个专门为重构设计的 Python 库。建议在重构过程中谨慎使用版本控制，如 `git`，以增量方式跟踪变化。

## 另请参阅
对于那些渴望获取更多信息的人，请深入以下资源：
- Martin Fowler 的书: [重构：改善既有代码的设计](http://www.refactoring.com/)
- 使用 `rope` 进行 Python 重构: [GitHub - rope](https://github.com/python-rope/rope)
- PyCharm 重构文档: [Jetbrains PyCharm 重构源代码](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [重构与设计模式](https://refactoring.guru/refactoring)
- Uncle Bob (Robert C. Martin) 的 Clean Code 讲座: [清洁代码 - Uncle Bob / 第1课](https://www.youtube.com/watch?v=7EmboKQH8lM)
