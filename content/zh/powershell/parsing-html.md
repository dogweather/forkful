---
title:                "解析HTML。"
html_title:           "PowerShell: 解析HTML。"
simple_title:         "解析HTML。"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/parsing-html.md"
---

{{< edit_this_page >}}

# 什么是HTML解析及其作用？
HTML解析是指从网页中提取信息的过程，它使程序员能够轻松获取网页内容并把它们转换成计算机可读的数据。这样，程序员就可以对这些数据进行分析和处理，以便为用户提供更好的体验。

# 如何进行HTML解析？
下面是一个例子，在PowerShell中使用Invoke-WebRequest命令进行HTML解析，并将解析后的结果打印出来。

```powershell
$html = Invoke-WebRequest "https://example.com"
$htmlParsed = $html.ParsedHtml

$htmlParsed.body.getElementsByTagName("p") | Select-Object innerText
```

这个代码会从"https://example.com"网页中提取所有段落的内容，并将它们打印出来。

# 深入探讨
在互联网早期，网页内容的提取是非常困难的，因为它们通常是编码复杂的二进制数据。随着HTML的出现，网页内容变得更加易读，解析网页也变得更加容易。除了PowerShell中使用的Invoke-WebRequest命令外，还有一些其他的解析HTML的工具，但它们可能需要更多的配置和学习成本。

# 参考链接
- [使用PowerShell进行WebScraping](https://docs.microsoft.com/zh-cn/archive/blogs/benjaminperkins/using-powershell-to-web-scrape-with-a-sql-database)
- [使用PowerShell及Regex来提取URL地址](https://devblogs.microsoft.com/scripting/use-powershell-and-regex-to-extract-urls-from-a-file/)
- [HTML解析的历史](https://www.w3.org/html/ig/history/)