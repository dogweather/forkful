---
date: 2024-02-03 19:03:00.218296-07:00
description: "Parsing HTML in PowerShell is about dissecting HTML content to extract\
  \ specific data or to automate web-related tasks. Programmers do it to interact\
  \ with\u2026"
lastmod: '2024-03-13T22:45:00.282032-06:00'
model: gpt-4-0125-preview
summary: "Parsing HTML in PowerShell is about dissecting HTML content to extract specific\
  \ data or to automate web-related tasks. Programmers do it to interact with\u2026"
title: Parsing HTML
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML in PowerShell is about dissecting HTML content to extract specific data or to automate web-related tasks. Programmers do it to interact with web pages, scrape web content, or automate form submissions and other web interactions without needing a web browser.

## How to:

PowerShell does not natively have a dedicated HTML parser, but you can utilize the `Invoke-WebRequest` cmdlet to access and parse HTML content. For more complex parsing and manipulation, the HtmlAgilityPack, a popular .NET library, can be employed.

### Using `Invoke-WebRequest`:

```powershell
# Simple example to fetch titles from a webpage
$response = Invoke-WebRequest -Uri 'http://example.com'
# Utilize the ParsedHtml property to access DOM elements
$title = $response.ParsedHtml.title
Write-Output $title
```

Sample Output:

```
Example Domain
```

### Using HtmlAgilityPack:

First, you need to install the HtmlAgilityPack. You can do this via NuGet Package Manager:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

Then, you can use it in PowerShell to parse HTML:

```powershell
# Load the HtmlAgilityPack assembly
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# Create an HtmlDocument object
$doc = New-Object HtmlAgilityPack.HtmlDocument

# Load HTML from a file or a web request
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# Use XPath or other query methods to extract elements
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

Sample Output:

```
Welcome to Example.com!
```

In these examples, `Invoke-WebRequest` is best for simple tasks, whereas HtmlAgilityPack offers a much richer set of features for complex HTML parsing and manipulation.
