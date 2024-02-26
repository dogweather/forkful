---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:07.478605-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u91CD\u6784\u6D89\u53CA\u4FEE\u6539\u4EE3\
  \u7801\u7684\u7ED3\u6784\u800C\u4E0D\u6539\u53D8\u5176\u884C\u4E3A\uFF0C\u4EE5\u6539\
  \u5584\u53EF\u8BFB\u6027\u3001\u53EF\u7EF4\u62A4\u6027\u6216\u6027\u80FD\u7B49\u65B9\
  \u9762\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u4F7F\u4EE3\
  \u7801\u66F4\u9AD8\u6548\u3001\u66F4\u6613\u4E8E\u7406\u89E3\u3001\u672A\u6765\u66F4\
  \u6613\u4E8E\u4FEE\u6539\uFF0C\u5E76\u51CF\u5C11\u51FA\u73B0\u9519\u8BEF\u7684\u53EF\
  \u80FD\u6027\u3002"
lastmod: '2024-02-25T18:49:45.146496-07:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u91CD\u6784\u6D89\u53CA\u4FEE\u6539\u4EE3\
  \u7801\u7684\u7ED3\u6784\u800C\u4E0D\u6539\u53D8\u5176\u884C\u4E3A\uFF0C\u4EE5\u6539\
  \u5584\u53EF\u8BFB\u6027\u3001\u53EF\u7EF4\u62A4\u6027\u6216\u6027\u80FD\u7B49\u65B9\
  \u9762\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u4F7F\u4EE3\
  \u7801\u66F4\u9AD8\u6548\u3001\u66F4\u6613\u4E8E\u7406\u89E3\u3001\u672A\u6765\u66F4\
  \u6613\u4E8E\u4FEE\u6539\uFF0C\u5E76\u51CF\u5C11\u51FA\u73B0\u9519\u8BEF\u7684\u53EF\
  \u80FD\u6027\u3002"
title: "\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么和为什么？

在编程中，重构涉及修改代码的结构而不改变其行为，以改善可读性、可维护性或性能等方面。程序员进行重构是为了使代码更高效、更易于理解、未来更易于修改，并减少出现错误的可能性。

## 如何操作：

考虑在Visual Basic for Applications (VBA)中的一个基础示例，我们有一个子程序用于打印员工的细节。最初，代码杂乱无章，难以维护或扩展。

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

重构步骤1：提取方法。最常用的重构技巧之一是将特定的代码片段移动到其自己的方法中。这使得代码更模块化，更易于理解。

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    DisplayMessage name, age, department
End Sub

Private Sub DisplayMessage(name As String, age As Integer, department As String)
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

重构步骤2：使用结构。这一步涉及使用数据结构来保存相关数据，提高代码清晰度并使得传递分组数据更加容易。

```vb
Type Employee
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Employee
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Employee)
    MsgBox "Name: " & emp.name & vbCrLf & "Age: " & emp.age & vbCrLf & "Department: " & emp.department
End Sub
```

这些步骤将杂乱的代码转变为模块化、结构化的代码，显著提高了可读性和可维护性。

## 深度探索

重构的概念与编程本身一样古老，但是是Martin Fowler的书《重构：改善既有代码的设计》将其推向了主流，强调了它在软件开发过程中的重要性。在Visual Basic for Applications中，由于缺乏更现代的集成开发环境（IDE）中提供的支持自动重构的内置工具，重构可能会有些更具挑战性。

然而，这并没有减少其重要性。即使在VBA中，手动应用基本的重构技巧也可以大大增强代码库，使其更清洁、更高效。虽然VBA可能没有相同的现代便利性，但良好代码设计的原则仍然是普遍适用的。来自其他语言的开发人员可能会发现手动过程乏味，但无疑会赞赏从一开始就投入时间改善代码质量的好处。

对于更健壮的开发环境或者在处理特别复杂的项目时，探索提供更强大重构工具的替代方案或将VBA项目转换为.NET语言可能是值得的，其中Visual Studio提供了广泛的重构支持。尽管如此，理解和应用VBA中的重构原则是一项宝贵的技能，强调了无论在何种环境下编写干净、可维护的代码的重要性。
