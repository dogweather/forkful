---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:02.649735-07:00
description: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u89E3\u6790 HTML \u6D89\
  \u53CA\u5230\u4ECE HTML \u6587\u6863\u4E2D\u63D0\u53D6\u7279\u5B9A\u4FE1\u606F\u3002\
  \u7A0B\u5E8F\u5458\u8FDB\u884C\u6B64\u64CD\u4F5C\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\
  \u4ECE\u7F51\u9875\u8BFB\u53D6\u548C\u5904\u7406\u6570\u636E\u7684\u8FC7\u7A0B\uFF0C\
  \u4F8B\u5982\u6293\u53D6\u7F51\u7AD9\u5185\u5BB9\u6216\u81EA\u52A8\u5316\u8868\u5355\
  \u63D0\u4EA4\u548C\u6570\u636E\u68C0\u7D22\uFF0C\u4EE5\u53CA\u5728\u652F\u6301 VBA\
  \ \u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u4F7F\u7528\uFF0C\u5982 Microsoft Excel\u2026"
lastmod: '2024-03-11T00:14:21.339723-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Visual Basic for Applications (VBA) \u4E2D\u89E3\u6790 HTML \u6D89\
  \u53CA\u5230\u4ECE HTML \u6587\u6863\u4E2D\u63D0\u53D6\u7279\u5B9A\u4FE1\u606F\u3002\
  \u7A0B\u5E8F\u5458\u8FDB\u884C\u6B64\u64CD\u4F5C\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\
  \u4ECE\u7F51\u9875\u8BFB\u53D6\u548C\u5904\u7406\u6570\u636E\u7684\u8FC7\u7A0B\uFF0C\
  \u4F8B\u5982\u6293\u53D6\u7F51\u7AD9\u5185\u5BB9\u6216\u81EA\u52A8\u5316\u8868\u5355\
  \u63D0\u4EA4\u548C\u6570\u636E\u68C0\u7D22\uFF0C\u4EE5\u53CA\u5728\u652F\u6301 VBA\
  \ \u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u4F7F\u7528\uFF0C\u5982 Microsoft Excel\u2026"
title: "\u89E3\u6790HTML"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Visual Basic for Applications (VBA) 中解析 HTML 涉及到从 HTML 文档中提取特定信息。程序员进行此操作是为了自动化从网页读取和处理数据的过程，例如抓取网站内容或自动化表单提交和数据检索，以及在支持 VBA 的应用程序中使用，如 Microsoft Excel 或 Access。

## 如何操作：

在 VBA 中，你可以使用 `Microsoft HTML Object Library` 来解析 HTML。通过在 VBA 编辑器中前往工具 > 引用，并勾选 `Microsoft HTML Object Library` 来添加对这个库的引用。这使你能够访问用于导航和操作 HTML 文档的类。

这里有一个简单的示例，展示了如何从文件加载 HTML 文档并提取所有链接（锚标签）：

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' 从文件加载 HTML 内容
    htmlFile = "C:\path\to\your\file.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' 初始化 HTML 文档
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' 获取所有锚标签
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' 循环遍历所有锚元素并打印 href 属性
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

此脚本读取 HTML 文件的内容，将其加载到 `HTMLDocument` 对象中，检索所有锚元素（`<a>` 标签），然后遍历它们，将每个元素的 `href` 属性打印到即时窗口。

## 深入探讨：

历史上，在 VBA 中解析 HTML 由于缺乏对现代网页抓取和文档处理技术的直接支持，而显得有些繁琐。Microsoft HTML Object Library 虽然功能强大，但有些过时，可能无法像较新的技术那样平滑地处理现代网络标准。

对于复杂的 HTML 解析和网页抓取任务，通常推荐使用 Python 以及像 Beautiful Soup 或 Scrapy 这样的库作为替代工具和语言。这些现代工具提供了更多的灵活性，更好的性能，并且更符合当前的网络标准。然而，在 Microsoft Office 生态系统中工作时，使用 VBA 和 Microsoft HTML Object Library 仍然是一项宝贵的技能。它以一种与 Excel 和 Access 等应用程序无缝集成的方式，解锁了对 HTML 内容的直接操控，为涉及基本的 HTML 文档处理的任务提供了一种简单的方法，无需离开熟悉的 VBA 环境。
