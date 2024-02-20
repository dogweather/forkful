---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:41.912430-07:00
description: "\u5904\u7406\u590D\u6570\u5305\u62EC\u5BF9\u5177\u6709\u5B9E\u90E8\u548C\
  \u865A\u90E8\u7684\u6570\u5B57\u6267\u884C\u6570\u5B66\u8FD0\u7B97\u3002\u7A0B\u5E8F\
  \u5458\u5728\u5DE5\u7A0B\u3001\u7269\u7406\u4EE5\u53CA\u6D89\u53CA\u5230\u89E3\u51B3\
  \u4EC5\u901A\u8FC7\u5B9E\u6570\u65E0\u6CD5\u89E3\u51B3\u7684\u65B9\u7A0B\u7684\u4EFB\
  \u4F55\u9886\u57DF\u4E2D\uFF0C\u7ECF\u5E38\u9700\u8981\u5904\u7406\u590D\u6570\u3002"
lastmod: 2024-02-19 22:05:06.584926
model: gpt-4-0125-preview
summary: "\u5904\u7406\u590D\u6570\u5305\u62EC\u5BF9\u5177\u6709\u5B9E\u90E8\u548C\
  \u865A\u90E8\u7684\u6570\u5B57\u6267\u884C\u6570\u5B66\u8FD0\u7B97\u3002\u7A0B\u5E8F\
  \u5458\u5728\u5DE5\u7A0B\u3001\u7269\u7406\u4EE5\u53CA\u6D89\u53CA\u5230\u89E3\u51B3\
  \u4EC5\u901A\u8FC7\u5B9E\u6570\u65E0\u6CD5\u89E3\u51B3\u7684\u65B9\u7A0B\u7684\u4EFB\
  \u4F55\u9886\u57DF\u4E2D\uFF0C\u7ECF\u5E38\u9700\u8981\u5904\u7406\u590D\u6570\u3002"
title: "\u5904\u7406\u590D\u6570\u7684\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么和为什么？

处理复数包括对具有实部和虚部的数字执行数学运算。程序员在工程、物理以及涉及到解决仅通过实数无法解决的方程的任何领域中，经常需要处理复数。

## 如何操作：

在Visual Basic for Applications（VBA）中，与支持复数的语言相比，处理复数可能不那么直接。然而，您可以通过创建函数或使用现有的库函数来管理复杂运算。让我们探索一个基本例子，包括复数的加法、减法、乘法和除法：

```vb
' 加法函数
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' 从复数中提取实部和虚部
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' 进行加法运算
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' 示例用法
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "加法结果: " & result  ' 输出: 加法结果: 4+9i
End Sub
```

虽然这演示了加法，但可以采取类似方法来适应减法、乘法和除法。对于超出基本算术的复杂运算，探索外部库或整合更本地支持复数操作的其他解决方案可能是值得的。

## 深入探讨：

VBA不包括对复数的内置支持，这是其落后于像Python（拥有一个复数类(`complex`)）或C++（其标准模板库`std::complex`）等语言的一个方面。从历史上看，直接在VBA中操作复数的需求相对较少，因为它通常用于自动化、操作Office应用程序以及传统上不要求复杂数学计算的任务。当VBA被构思和开发时，其用例主要集中在商业应用而非科学计算，这可能解释了此遗漏。

对于需要大量复数操作的任务，程序员可能会发现使用更注重数学的语言是有益的。然而，对于那些致力于或受限于使用VBA的人来说，编写自定义函数（如示例所示）或与具有这些能力的软件（如MATLAB或在某种程度上的Excel本身）集成，是可行的途径。尽管有其局限性，创造性的解决方案和外部集成可以将VBA的实用性扩展到它最初未被设计的领域，包括处理复数。
