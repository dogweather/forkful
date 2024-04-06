---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:39.550825-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PowerShell \u6CA1\u6709\u539F\u751F\u652F\
  \u6301\u7684\u4E13 dedicated \u7684 HTML \u89E3\u6790\u5668\uFF0C\u4F46\u4F60\u53EF\
  \u4EE5\u4F7F\u7528 `Invoke-WebRequest` cmdlet \u6765\u8BBF\u95EE\u548C\u89E3\u6790\
  \ HTML \u5185\u5BB9\u3002\u5BF9\u4E8E\u66F4\u590D\u6742\u7684\u89E3\u6790\u548C\u64CD\
  \u4F5C\uFF0C\u53EF\u4EE5\u4F7F\u7528 HtmlAgilityPack\uFF0C\u8FD9\u662F\u4E00\u4E2A\
  \u6D41\u884C\u7684 .NET \u5E93\u3002"
lastmod: '2024-03-13T22:44:48.011088-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u6CA1\u6709\u539F\u751F\u652F\u6301\u7684\u4E13 dedicated \u7684\
  \ HTML \u89E3\u6790\u5668\uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528 `Invoke-WebRequest`\
  \ cmdlet \u6765\u8BBF\u95EE\u548C\u89E3\u6790 HTML \u5185\u5BB9\u3002\u5BF9\u4E8E\
  \u66F4\u590D\u6742\u7684\u89E3\u6790\u548C\u64CD\u4F5C\uFF0C\u53EF\u4EE5\u4F7F\u7528\
  \ HtmlAgilityPack\uFF0C\u8FD9\u662F\u4E00\u4E2A\u6D41\u884C\u7684 .NET \u5E93."
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何操作：
PowerShell 没有原生支持的专 dedicated 的 HTML 解析器，但你可以使用 `Invoke-WebRequest` cmdlet 来访问和解析 HTML 内容。对于更复杂的解析和操作，可以使用 HtmlAgilityPack，这是一个流行的 .NET 库。

### 使用 `Invoke-WebRequest`：
```powershell
# 简单示例，从网页获取标题
$response = Invoke-WebRequest -Uri 'http://example.com'
# 使用 ParsedHtml 属性访问 DOM 元素
$title = $response.ParsedHtml.title
Write-Output $title
```

示例输出：

```
Example Domain
```

### 使用 HtmlAgilityPack：
首先，你需要安装 HtmlAgilityPack。你可以通过 NuGet 包管理器进行安装：

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

然后，你可以在 PowerShell 中使用它来解析 HTML：

```powershell
# 加载 HtmlAgilityPack 程序集
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# 创建 HtmlDocument 对象
$doc = New-Object HtmlAgilityPack.HtmlDocument

# 从文件或网络请求加载 HTML
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# 使用 XPath 或其他查询方法提取元素
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

示例输出：

```
Welcome to Example.com!
```

在这些示例中，`Invoke-WebRequest` 最适合简单任务，而 HtmlAgilityPack 为复杂的 HTML 解析和操作提供了更丰富的功能集。
