---
date: 2024-01-26 03:36:59.087763-07:00
description: "\u91CD\u6784\u662F\u91CD\u7EC4\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\
  \u7684\u8FC7\u7A0B\u2014\u2014\u6539\u53D8\u4EE3\u7801\u7684\u6784\u9020\u2014\u2014\
  \u4F46\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u6E05\u7406\u4EE3\u7801\u3001\u63D0\u9AD8\u53EF\u8BFB\
  \u6027\uFF0C\u4F7F\u5176\u66F4\u6613\u4E8E\u7EF4\u62A4\u548C\u6269\u5C55\uFF0C\u800C\
  \u4E0D\u662F\u6DFB\u52A0\u65B0\u529F\u80FD\u3002"
lastmod: '2024-02-25T18:49:44.896743-07:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u91CD\u7EC4\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\
  \u7684\u8FC7\u7A0B\u2014\u2014\u6539\u53D8\u4EE3\u7801\u7684\u6784\u9020\u2014\u2014\
  \u4F46\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u6E05\u7406\u4EE3\u7801\u3001\u63D0\u9AD8\u53EF\u8BFB\
  \u6027\uFF0C\u4F7F\u5176\u66F4\u6613\u4E8E\u7EF4\u62A4\u548C\u6269\u5C55\uFF0C\u800C\
  \u4E0D\u662F\u6DFB\u52A0\u65B0\u529F\u80FD\u3002"
title: "\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么 & 为什么？
重构是重组现有计算机代码的过程——改变代码的构造——但不改变其外部行为。程序员这样做是为了清理代码、提高可读性，使其更易于维护和扩展，而不是添加新功能。

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
