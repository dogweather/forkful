---
title:                "Парсинг HTML"
date:                  2024-01-20T15:33:14.281024-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Parsing HTML means extracting data from HTML content. Programmers do it to automate data retrieval from websites, like scraping prices or headlines.

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
