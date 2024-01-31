---
title:                "Парсинг HTML"
date:                  2024-01-20T15:30:55.331384-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Parsing HTML means you're extracting useful info from a web page's HTML code. We do it to automate data collection or to interact with websites programmatically.

## How to: (Як це зробити:)
**Installing HtmlAgilityPack:**

```csharp
// Use NuGet to install the HtmlAgilityPack package.
// Run this command in your Package Manager Console.
Install-Package HtmlAgilityPack
```

**Basic parsing example:**

```csharp
using System;
using HtmlAgilityPack;

class Program
{
    static void Main()
    {
        // Load the HTML document
        var web = new HtmlWeb();
        var doc = web.Load("http://example.com");

        // Parse the titles from the website
        foreach (var node in doc.DocumentNode.SelectNodes("//title"))
        {
            Console.WriteLine("Title: " + node.InnerText);
        }
    }
}
```

**Sample output:**

```
Title: Example Domain
```

## Deep Dive (Поглиблений аналіз):
**Historical Context:** Initially, HTML parsing relied on regular expressions and custom parsers. Tools like HtmlAgilityPack provide a DOM-like interface making it more robust against malformed HTML.

**Alternatives:** Besides HtmlAgilityPack, you can use AngleSharp or even regular expressions for simpler tasks. Some prefer using browser automation tools like Selenium for more complex scenarios.

**Implementation Details:** HtmlAgilityPack allows you to perform standard DOM operations, like selecting nodes via XPath or CSS Selectors. It's crucial to manage errors properly, as real-world HTML can often be quite far from standards compliant.

## See Also (Дивіться також):
- HtmlAgilityPack GitHub Repository: [https://github.com/zzzprojects/html-agility-pack](https://github.com/zzzprojects/html-agility-pack)
- AngleSharp GitHub Repository: [https://github.com/AngleSharp/AngleSharp](https://github.com/AngleSharp/AngleSharp)
- Introduction to XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
