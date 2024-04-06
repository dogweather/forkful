---
date: 2024-01-26 04:34:19.703762-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A XML \u6216\u53EF\u6269\u5C55\u6807\u8BB0\
  \u8BED\u8A00\uFF0C\u81EA 90 \u5E74\u4EE3\u672B\u8D77\u5C31\u5DF2\u7ECF\u5B58\u5728\
  \uFF0C\u5B83\u4ECD\u7136\u662F\u4E00\u79CD\u88AB\u5E7F\u6CDB\u4F7F\u7528\u7684\u7ED3\
  \u6784\u5316\u6570\u636E\u683C\u5F0F\u3002\u4E0E\u4F20\u7EDF\u7684\u89E3\u6790\u65B9\
  \u6CD5\u76F8\u6BD4\uFF0CPowerShell \u7B80\u5316\u4E86\u5904\u7406 XML \u7684\u8FC7\
  \u7A0B\uFF1B\u5B83\u76F4\u63A5\u5C06 XML \u8F6C\u6362\u4E3A\u5BF9\u8C61\uFF0C\u8BA9\
  \u4F60\u901A\u8FC7\u719F\u6089\u7684\u70B9\u8868\u793A\u6CD5\u4E0E\u5143\u7D20\u4EA4\
  \u4E92\u3002 XML \u7684\u66FF\u4EE3\u54C1\u5305\u62EC JSON\u3001YAML\u2026"
lastmod: '2024-04-05T21:53:48.338137-06:00'
model: gpt-4-0125-preview
summary: "XML \u7684\u66FF\u4EE3\u54C1\u5305\u62EC JSON\u3001YAML \u6216\u81EA\u5B9A\
  \u4E49\u6570\u636E\u683C\u5F0F\u3002\u4F8B\u5982\uFF0CJSON \u56E0\u5176\u8F7B\u91CF\
  \u7EA7\u6027\u8D28\u4EE5\u53CA\u4E0E\u7F51\u7EDC\u6280\u672F\u7684\u6613\u7528\u6027\
  \u800C\u83B7\u5F97\u4E86\u666E\u53CA\u3002\u7136\u800C\uFF0CXML \u7684\u6269\u5C55\
  \u7279\u6027\uFF0C\u5982\u547D\u540D\u7A7A\u95F4\u3001\u6A21\u5F0F\u548C XSLT \u5904\
  \u7406\uFF0C\u901A\u5E38\u4F7F\u5176\u66F4\u9002\u5408\u590D\u6742\u6587\u6863\u6216\
  \u884C\u4E1A\u6807\u51C6."
title: "\u5904\u7406XML"
weight: 40
---

## 如何操作：
```PowerShell
# 将一个 XML 文件加载到变量中
[xml]$xmlContent = Get-Content 'path\to\your\file.xml'

# 访问 XML 节点
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "标题: $($book.title)"
}

# 创建一个新的 XML 元素
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# 将 XML 保存回文件
$xmlContent.Save('path\to\your\updated\file.xml')
```
示例输出：
```
标题: 编程 PowerShell
标题: XML 基础
```

## 深入了解
XML 或可扩展标记语言，自 90 年代末起就已经存在，它仍然是一种被广泛使用的结构化数据格式。与传统的解析方法相比，PowerShell 简化了处理 XML 的过程；它直接将 XML 转换为对象，让你通过熟悉的点表示法与元素交互。

XML 的替代品包括 JSON、YAML 或自定义数据格式。例如，JSON 因其轻量级性质以及与网络技术的易用性而获得了普及。然而，XML 的扩展特性，如命名空间、模式和 XSLT 处理，通常使其更适合复杂文档或行业标准。

PowerShell 使用 .NET Framework 的 XML 功能来处理 XML。这意味着它不仅仅是关于简单的读写操作；你还可以使用 XML 模式进行验证，使用 XPath 进行查询，以及通过 PowerShell 进行 XSLT 转换。

## 另请参阅
- [W3Schools XML 教程](https://www.w3schools.com/xml/)
- [XML 与 JSON](https://www.json.org/json-en.html)
