---
aliases:
- /zh/vba/working-with-xml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:40.985842-07:00
description: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u5904\u7406 XML \u6D89\
  \u53CA\u5728 Microsoft Office \u5E94\u7528\u7A0B\u5E8F\u7684\u4E0A\u4E0B\u6587\u4E2D\
  \u89E3\u6790\u3001\u521B\u5EFA\u548C\u4FEE\u6539 XML \u6587\u6863\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u4F1A\u5229\u7528\u8FD9\u4E00\u80FD\u529B\uFF0C\u662F\u56E0\
  \u4E3A\u4ED6\u4EEC\u5E0C\u671B\u5C06 Office \u5E94\u7528\u7A0B\u5E8F\u4E0E\u53D1\
  \u51FA XML\u2026"
lastmod: 2024-02-18 23:08:59.005073
model: gpt-4-0125-preview
summary: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u5904\u7406 XML \u6D89\
  \u53CA\u5728 Microsoft Office \u5E94\u7528\u7A0B\u5E8F\u7684\u4E0A\u4E0B\u6587\u4E2D\
  \u89E3\u6790\u3001\u521B\u5EFA\u548C\u4FEE\u6539 XML \u6587\u6863\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u4F1A\u5229\u7528\u8FD9\u4E00\u80FD\u529B\uFF0C\u662F\u56E0\
  \u4E3A\u4ED6\u4EEC\u5E0C\u671B\u5C06 Office \u5E94\u7528\u7A0B\u5E8F\u4E0E\u53D1\
  \u51FA XML\u2026"
title: "\u5904\u7406XML"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

在 Visual Basic for Applications (VBA) 中处理 XML 涉及在 Microsoft Office 应用程序的上下文中解析、创建和修改 XML 文档。程序员之所以会利用这一能力，是因为他们希望将 Office 应用程序与发出 XML 的网络服务或其他数据源集成起来，以便促进数据交换和报告功能。

## 如何操作：

通常，人们会使用 `MSXML2.DOMDocument` 对象来开始与 XML 的交互。这个接口使你能够加载、解析和导航 XML 文档。下面是一个简单的示例，展示了如何加载 XML 文件，导航其结构，以及读取属性和文本内容。

```basic
' 首先，确保你通过 Tools -> References 添加了对 "Microsoft XML, v6.0" 的引用
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Path\To\Your\File.xml") ' 加载你的 XML 文件

' 检查 XML 是否成功加载
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "加载 XML 出错：" & xmlDoc.parseError.reason
Else
    ' 导航并读取元素
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath 查找 <book> 内的第一个 <title>
    MsgBox book.Text ' 显示标题文本
End If
```

在上面的示例代码中，我们创建了一个 `MSXML2.DOMDocument60` 的实例，加载了一个 XML 文件，然后检查错误。如果没有发现错误，我们使用 XPath 导航到一个特定节点并显示其文本内容。

## 深入探讨：

VBA 中集成 XML 功能可以追溯到 2000 年代初期，当时 Office 应用程序需要与网络数据和服务进行交互的需求开始增长。`MSXML` 库，或称微软 XML 核心服务，多年来已经发展过来，其中 `MSXML2.DOMDocument60` 是推荐使用的最新版本之一，因为它在性能和安全性方面有所改进。

尽管功能强大，但与 Python 的 XML.etree 或 C# 的 LINQ to XML 等现代编程环境相比，VBA 的 XML 处理能力被认为效率较低且更加繁琐。VBA 的固有冗长性和手动添加及管理引用的要求可能会阻碍快速开发。此外，随着 JSON 这种更轻量的数据交换格式的出现，许多程序员和应用程序都在从 XML 转向，除非与遗留系统或特定企业服务的互操作性需要使用它。

然而，对于需要在 Microsoft Office 自动化的上下文中解析或生成 XML 文档的任务，利用 VBA 的 XML 处理功能仍然是一种可行且有时是必要的方法。这在访问 Office 应用程序的丰富功能集和 XML 提供的结构化数据操作能力之间取得了平衡。
