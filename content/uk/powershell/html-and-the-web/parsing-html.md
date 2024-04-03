---
date: 2024-01-20 15:33:14.281024-07:00
description: "How to (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438): Install `AngleSharp`, a .NET library, using PowerShell."
lastmod: '2024-03-13T22:44:49.646626-06:00'
model: unknown
summary: Install `AngleSharp`, a .NET library, using PowerShell.
title: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML"
weight: 43
---

## How to (Як це зробити):
Install `AngleSharp`, a .NET library, using PowerShell:

```PowerShell
Install-Package AngleSharp
```

Example of parsing an HTML string to grab all `h1` elements:

```PowerShell
Add-Type -Path "path\to\AngleSharp.dll"

$html = @"
<html>
<head><title>Test</title></head>
<body>
    <h1>Heading 1</h1>
    <h1>Heading 2</h1>
    <p>Hello, world!</p>
</body>
</html>
"@

$parser = New-Object AngleSharp.Html.Parser.HtmlParser
$document = $parser.ParseDocument($html)
$headings = $document.QuerySelectorAll("h1")

foreach ($h in $headings) {
    Write-Output $h.TextContent
}
```

Sample output:

```
Heading 1
Heading 2
```

## Deep Dive (Глибоке занурення):
Historically, HTML parsing in PowerShell relied on Internet Explorer COM objects or regex hacks, but this was unreliable. AngleSharp, a modern .NET library, provides a robust and standards-compliant way to parse HTML. Other alternatives include HtmlAgilityPack and CsQuery. AngleSharp parses HTML into a Document Object Model (DOM) that you can query, making it similar to JavaScript's `document`.

## See Also (Дивіться також):
- AngleSharp GitHub Page: https://github.com/AngleSharp/AngleSharp
- HtmlAgilityPack GitHub Page: https://github.com/zzzprojects/html-agility-pack
- PowerShell Gallery: https://www.powershellgallery.com/
- DOM Documentation: https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model
