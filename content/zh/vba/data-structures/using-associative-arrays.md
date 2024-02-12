---
title:                "使用关联数组"
aliases:
- /zh/vba/using-associative-arrays.md
date:                  2024-02-01T22:04:38.764807-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

关联数组在 Visual Basic for Applications (VBA) 中通常被称为字典，允许程序员创建键值对集合。这一特性对于高效的数据存储和检索至关重要，提供了一种比传统数组索引更灵活直观的数据管理方式。

## 如何操作：

在 VBA 中，`Dictionary` 对象提供了类似于关联数组的功能。你必须首先添加对 Microsoft Scripting Runtime 的引用才能使用它：

1. 在 VBA 编辑器中，转至 工具 > 引用...
2. 勾选 “Microsoft Scripting Runtime” 并点击确定。

下面是如何声明、填充和访问 `Dictionary` 中的项目：

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' 添加项目
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' 访问项目
Debug.Print sampleDictionary.Item("Name")  ' 输出: John Doe
Debug.Print sampleDictionary.Item("Age")   ' 输出: 29

' 检查键是否存在
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation 键存在"
End If

' 移除项目
sampleDictionary.Remove("Occupation")

' 遍历字典
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## 深入了解

`Dictionary` 对象在底层接口上与 Windows 脚本宿主的组件对接。因此，它是一个后期绑定的 COM 对象，这曾是过去扩展 VBA 功能的常见方式。其在 VBA 中的使用可以显著增强语言处理复杂数据集的能力，而不强制执行传统数组或 Excel 范围中看到的刚性结构。

需要注意的一个限制是访问 `Dictionary` 需要设置对 Microsoft Scripting Runtime 的引用，这可能会使分发你的 VBA 项目复杂化。VBA 内部存在替代品如 Collections，但缺少一些 `Dictionary` 的关键特性，例如在不触发错误的情况下轻松检查键是否存在的能力。

在更现代的编程环境中，像 Python 这样的语言提供了对关联数组（在 Python 中也称为字典）的内置支持，无需添加外部引用。这种内置支持简化了过程，并提供了更多高级功能。然而，在 VBA 的限制内，以及针对自动化 Microsoft Office 套件任务的特定应用程序中，使用 `Dictionary` 对象仍然是一种强大且相关的方法，用于类似关联数组的数据结构。
