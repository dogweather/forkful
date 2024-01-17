---
title:                "打印调试输出"
html_title:           "Python: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/printing-debug-output.md"
---

{{< edit_this_page >}}

Python Debug 输出：为程序员的简易指南

## 什么？为什么？

打印调试输出是指将程序中的运行信息打印出来，以帮助程序员调试代码和发现错误。程序员经常会通过打印输出来确定代码的执行流程和变量的值，从而更容易地进行修复和优化。

## 如何：

```Python
# 使用 print() 函数打印变量的值
name = "张三"
print(name)

# 打印运算结果
num1 = 3
num2 = 5
print(num1 + num2)

# 打印调试信息
def add_nums(a, b):
  res = a + b
  print("a的值为：" + str(a))
  print("b的值为：" + str(b))
  print("a和b的和为：" + str(res))
  return res

add_nums(2, 4)
```

输出：

张三
8
a的值为：2
b的值为：4
a和b的和为：6

## 深入了解

打印调试输出在计算机科学与编程中有着悠久的历史。在早期的程序设计中，打印输出是唯一的调试工具。如今，随着计算技术的发展，也出现了更多高级的调试工具，但是打印输出仍然是程序员最常用的调试方式之一。

除了使用 print() 函数，程序员还可以使用 logging 模块来打印调试信息。logging 模块提供了更多的调试选项和更灵活的日志管理功能。

在编写代码时，更推荐使用打印调试输出来确定程序的执行流程和变量的值，因为它是一种简单和直观的方式。

## 参考资料

- [Python 官方文档：打印调试输出](https://docs.python.org/zh-cn/3.9/library/functions.html#print)
- [logging 模块官方文档](https://docs.python.org/zh-cn/3.9/library/logging.html)