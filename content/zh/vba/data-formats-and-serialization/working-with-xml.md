---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:40.985842-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u901A\u5E38\uFF0C\u4EBA\u4EEC\u4F1A\u4F7F\
  \u7528 `MSXML2.DOMDocument` \u5BF9\u8C61\u6765\u5F00\u59CB\u4E0E XML \u7684\u4EA4\
  \u4E92\u3002\u8FD9\u4E2A\u63A5\u53E3\u4F7F\u4F60\u80FD\u591F\u52A0\u8F7D\u3001\u89E3\
  \u6790\u548C\u5BFC\u822A XML \u6587\u6863\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u7B80\
  \u5355\u7684\u793A\u4F8B\uFF0C\u5C55\u793A\u4E86\u5982\u4F55\u52A0\u8F7D XML \u6587\
  \u4EF6\uFF0C\u5BFC\u822A\u5176\u7ED3\u6784\uFF0C\u4EE5\u53CA\u8BFB\u53D6\u5C5E\u6027\
  \u548C\u6587\u672C\u5185\u5BB9\u3002"
lastmod: '2024-04-05T21:53:47.927267-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5904\u7406XML"
weight: 40
---

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
