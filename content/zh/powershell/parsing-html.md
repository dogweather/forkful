---
title:                "解析HTML"
date:                  2024-01-20T15:33:30.436730-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
解析HTML就是提取网页的结构化内容。程序员这么做是为了自动化地处理网页数据，比如数据挖掘或内容迁移。

## How to: (如何实现：)
使用PowerShell解析HTML，可以借助`HtmlAgilityPack`库。首先，安装该库：

```PowerShell
Install-Package HtmlAgilityPack
```

然后，加载HTML，提取信息：

```PowerShell
# 加载HtmlAgilityPack
Add-Type -Path "path/to/HtmlAgilityPack.dll"

# 读取HTML文件
$html = New-Object HtmlAgilityPack.HtmlDocument
$html.Load('path/to/yourfile.html')

# 选择并输出所有<h1>标签的内容
$h1Tags = $html.DocumentNode.SelectNodes('//h1')
foreach ($tag in $h1Tags) {
    Write-Output $tag.InnerText
}
```

如果你的HTML是网络上的，可以这样：

```PowerShell
# 使用WebClient下载HTML
$webClient = New-Object System.Net.WebClient
$htmlContent = $webClient.DownloadString('http://example.com')

# 加载内容到HtmlDocument
$html.LoadHtml($htmlContent)

# 同样提取<h1>
$h1Tags = $html.DocumentNode.SelectNodes('//h1')
$h1Tags | ForEach-Object { Write-Output $_.InnerText }
```

## Deep Dive (深入了解)
HTML解析不是个新话题。长期以来，人们一直在寻找更高效、准确的方法解析网页。`HtmlAgilityPack`是目前用于.NET环境的解析库中较受欢迎的一个。它处理不规则标记，并提供XPath或CSS选择器来查找元素。

其他方式比如使用正则表达式，但这通常不推荐，因为它对于复杂的HTML结构不够灵活和可靠。PowerShell本身没有内建的HTML解析库，所以使用外部库是常见选择。

在实际的应用中，除了提取文本，我们还可以通过解析HTML来修改元素、删除节点或添加新的内容，使其成为强大的自动化工具。

## See Also (另请参阅)
- HtmlAgilityPack官方文档: [https://html-agility-pack.net/](https://html-agility-pack.net/)
- XPath教程: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)