---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/parsing-html.md"
---

{{< edit_this_page >}}

# HTML Parsing in PowerShell: A Practical Guide

## What & Why?
HTML Parsing lets you extract specific data from a webpage. Programmers do it for web scraping, to mechanically process data, automate tasks, or gather information from the web.

## How to:
PowerShell, along with the Html Agility Pack, makes HTML parsing easy. First, you need to install the 'HtmlAgilityPack' module.

```PowerShell
Install-Package HtmlAgilityPack
```

Then you can load a webpage into an 'HtmlDocument' object, as follows:

```PowerShell
# Load HtmlAgilityPack
Add-Type -Path 'path-to-HtmlAgilityPack.dll'

# Load a webpage
$Web = New-Object HtmlAgilityPack.HtmlWeb
$Doc = $Web.Load('https://your-target-website.com')

# Extract all Paragraphs
$Paragraphs = $Doc.DocumentNode.SelectNodes('//p')

# Print out the InnerText of each Paragraph
$Paragraphs | ForEach-Object { Write-Output $_.InnerText }
```

This code will output the text contained in all paragraphs ('<p>') on the specified webpage.

## Deep Dive
HTML Parsing has been around since the early days of the internet. Early implementations were often crude, relying on Regular Expressions, which can be error-prone and difficult for complex HTML structures.

PowerShell, combined with the robust Html Agility Pack, provides a more powerful and flexible solution. The Html Agility Pack is an open-source .NET library that can parse malformed HTML, like the real-world kind!

An alternative to Html Agility Pack is using the native .NET `System.Xml.XmlDocument` object. However, it's often less forgiving of non-standard or incorrect HTML code.

Furthermore, the implementation of HTML Parsing in PowerShell uses .NET's Document Object Model (DOM). DOM allows developers to traverse and modify the HTML as if it's a tree-like database of the elements on the page. 

## See Also
For more in-depth knowledge on HTML Parsing with PowerShell, check out these useful resources:

1. [Html Agility Pack GitHub](https://github.com/zzzprojects/html-agility-pack)
2. [Microsoft's guide on XmlDocument](https://docs.microsoft.com/en-us/dotnet/api/system.xml.xmldocument?view=net-5.0)
4. [Introduction to the DOM](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)

Embrace HTML Parsing in PowerShell! It's an invaluable skill when you need to handle web data, and there's ample help and resources out there.