---
title:                "将代码组织成函数"
aliases:
- /zh/vba/organizing-code-into-functions/
date:                  2024-02-01T21:57:07.386966-07:00
model:                 gpt-4-0125-preview
simple_title:         "将代码组织成函数"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/organizing-code-into-functions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Visual Basic for Applications（VBA）中将代码组织成函数涉及将程序分解成称为函数的更小、可管理的片段。程序员这样做是为了增强代码可读性，高效重用代码，以及简化调试和维护过程。

## 如何操作：

在VBA中，函数使用`Function`和`End Function`语句定义。下面是一个简单例子，展示了如何创建一个计算矩形面积的函数：

```basic
Function CalculateArea(length As Double, width As Double) As Double
    CalculateArea = length * width
End Function
```

要在VBA代码中调用此函数并在消息框中显示结果，你会使用：

```basic
Sub ShowArea()
    Dim area As Double
    area = CalculateArea(10, 5)
    MsgBox "The area is " & area
End Sub
```

执行这段代码时，会显示一个消息框，说明：`面积是50`。

### 通过引用(ByRef)和按值传递(ByVal)变量

VBA允许你通过引用(`ByRef`)或按值(`ByVal`)向函数传递变量。前者意味着原始变量可以被函数修改，而后者传递一个副本，保护原始变量不受更改。

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## 深入探讨

VBA作为一种事件驱动的编程语言，对函数和子程序的使用强调不同的任务处理。不同于许多现代语言，VBA具有一个独特的特性，其中`Function`关键字不仅声明了一块可重复使用的代码块，还允许直接将隐式返回值分配给函数的名称。

从历史上看，VBA函数的设计受到了早期编程范式的影响，封装和模块化的重要性逐渐被认识到。这一历史背景导致VBA采取了一种相对保守但功能性的方法来组织代码。

虽然VBA在其原生环境（例如，Microsoft Office应用程序）内非常强大，但值得注意的是，编程世界已经发展。像Python这样的语言提供了更简单的语法和广泛的标准库，使它们成为Office套件之外各种应用程序的有利选择。然而，当在Microsoft Office产品中工作时，VBA提供的集成和自动化能力是无与伦比的。

值得一提的是，尽管VBA的年龄已长，围绕VBA的社区仍然保持活跃，不断找到创新的方法来利用其功能。然而，随着软件行业向更现代化、多功能和稳健的语言转变，鼓励熟悉VBA的程序员探索这些替代语言，以用于非Office相关任务，以拓宽他们的编码工具包。
