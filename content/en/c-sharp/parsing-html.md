---
title:                "Parsing HTML"
date:                  2024-01-20T15:30:30.757483-07:00
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML means extracting info from HTML documents. Programmers do it to interact with web content programmatically, scrape data, or automate web interactions. 

## How to:
Let's go with a popular .NET library for HTML parsing: HtmlAgilityPack. 

First, install it via NuGet:
```shell
Install-Package HtmlAgilityPack
```

Next, load an HTML doc and grab some nodes:

```C#
using System;
using HtmlAgilityPack;

class Program
{
    static void Main()
    {
        var web = new HtmlWeb();
        var doc = web.Load("http://example.com");

        foreach (var node in doc.DocumentNode.SelectNodes("//a[@href]"))
        {
            Console.WriteLine($"Text: {node.InnerText}, Link: {node.Attributes["href"].Value}");
        }
    }
}
```
The snippet above fetches all anchor tags with an `href` attribute and prints their text and link.

Sample output might look like this:

```
Text: Home, Link: http://example.com/home
Text: About, Link: http://example.com/about
...
```

## Deep Dive
HtmlAgilityPack (HAP) has been top dog for parsing since the early 2000s. It's loved for its flexibility and ease of use, closely mimicking the DOM in browsers.

Alternatives? Sure. AngleSharp is a newer library, with async support and follows current web standards closer. For simple tasks, you could even use Regex, but be warned - HTML wasn't made to be regex-friendly. It's a hacky solution at best.

Implementation-wise, HAP parses the given HTML into a DOM-like structure, letting you query and manipulate nodes using XPath or LINQ. It's robust enough to handle wonky HTML, giving it an edge in scraping real-world, often imperfect webpages.

## See Also
- HtmlAgilityPack on GitHub: [https://github.com/zzzprojects/html-agility-pack](https://github.com/zzzprojects/html-agility-pack)
- AngleSharp GitHub & docs: [https://github.com/AngleSharp/AngleSharp](https://github.com/AngleSharp/AngleSharp)
- Web scraping best practices article: (link to a reputable source with guidelines and legality of web scraping).
