---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:38.764807-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 VBA \u4E2D\uFF0C`Dictionary` \u5BF9\
  \u8C61\u63D0\u4F9B\u4E86\u7C7B\u4F3C\u4E8E\u5173\u8054\u6570\u7EC4\u7684\u529F\u80FD\
  \u3002\u4F60\u5FC5\u987B\u9996\u5148\u6DFB\u52A0\u5BF9 Microsoft Scripting Runtime\
  \ \u7684\u5F15\u7528\u624D\u80FD\u4F7F\u7528\u5B83\uFF1A 1. \u5728 VBA \u7F16\u8F91\
  \u5668\u4E2D\uFF0C\u8F6C\u81F3 \u5DE5\u5177 > \u5F15\u7528... 2. \u52FE\u9009 \u201C\
  Microsoft Scripting Runtime\u201D\u2026"
lastmod: '2024-03-13T22:44:47.563500-06:00'
model: gpt-4-0125-preview
summary: "\u5728 VBA \u4E2D\uFF0C`Dictionary` \u5BF9\u8C61\u63D0\u4F9B\u4E86\u7C7B\
  \u4F3C\u4E8E\u5173\u8054\u6570\u7EC4\u7684\u529F\u80FD\u3002\u4F60\u5FC5\u987B\u9996\
  \u5148\u6DFB\u52A0\u5BF9 Microsoft Scripting Runtime \u7684\u5F15\u7528\u624D\u80FD\
  \u4F7F\u7528\u5B83\uFF1A\n\n1."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
