---
date: 2024-01-26 01:16:14.030072-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5047\u8BBE\u4F60\u6B63\u5728\u7F16\u5199\
  \u4E00\u4E2A\u811A\u672C\u6765\u8BA1\u7B97\u4E00\u4E2A\u6570\u5B57\u7684\u5E73\u65B9\
  \u548C\u7ACB\u65B9\u3002\u4E0D\u4F7F\u7528\u51FD\u6570\uFF0C\u4F1A\u662F\u4E00\u56E2\
  \u4E71\u9EBB\u7684\u91CD\u590D\uFF1A."
lastmod: '2024-03-13T22:44:47.261855-06:00'
model: gpt-4-0125-preview
summary: "\u5047\u8BBE\u4F60\u6B63\u5728\u7F16\u5199\u4E00\u4E2A\u811A\u672C\u6765\
  \u8BA1\u7B97\u4E00\u4E2A\u6570\u5B57\u7684\u5E73\u65B9\u548C\u7ACB\u65B9\u3002\u4E0D\
  \u4F7F\u7528\u51FD\u6570\uFF0C\u4F1A\u662F\u4E00\u56E2\u4E71\u9EBB\u7684\u91CD\u590D\
  \uFF1A."
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何操作：
假设你正在编写一个脚本来计算一个数字的平方和立方。不使用函数，会是一团乱麻的重复：

```Python
num = 4
square = num * num
cube = num * num * num
print(f"Square: {square}, Cube: {cube}")

num = 5
square = num * num
cube = num * num * num
print(f"Square: {square}, Cube: {cube}")
```
输出：
```
Square: 16, Cube: 64
Square: 25, Cube: 125
```

使用函数，会更整洁：

```Python
def square(n):
    return n * n

def cube(n):
    return n ** 3

num = 4
print(f"Square: {square(num)}, Cube: {cube(num)}")

num = 5
print(f"Square: {square(num)}, Cube: {cube(num)}")
```
输出：
```
Square: 16, Cube: 64
Square: 25, Cube: 125
```

## 深入探究
早年间，当程序还很简单的时候，只写一列指令或许可以应付。但随着软件变得更复杂，开发者们意识到他们一次又一次地重写相同的代码。于是，函数登场了——执行单一操作的可重用代码块。

函数的替代品包括类（将函数与它们操作的数据捆绑）和内联代码（就在你需要的地方提供智能，但对于复杂任务来说风险很大）。在实现上，技巧不仅仅在于创建函数，而是要使它们做好一件事 —— 想想单一职责原则。理想情况下，函数应该是无状态的，意味着数据进出没有惊喜。

## 另请参见
- 官方Python关于函数的教程：https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 由罗伯特·C·马丁撰写的《代码整洁之道》, 关于如何编写整洁函数的原则。
- 马丁·福勒的《重构：改善既有代码的设计》，其中包含了组织代码的示例。
