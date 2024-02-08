---
title:                "解析HTML"
date:                  2024-02-03T19:12:39.550825-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 PowerShell 中解析 HTML 意味着解析 HTML 内容以提取特定数据或自动化与网络相关的任务。程序员这样做是为了与网页交互、抓取网页内容，或者自动化表单提交和其他网络交互，而不需要网络浏览器。

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
