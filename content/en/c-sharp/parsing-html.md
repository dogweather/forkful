---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML involves reading HTML code and understanding its structure. It's important for data scraping, automated web browsing, and even testing - to pull out specific information or automate interactions with websites.

## How To:

Here's how you'd parse HTML in C# - using the Agility Pack. First, download it through the Nuget Package Manager in your IDE.

Here's a concise example:
```
C#
var web = new HtmlWeb();
var document = web.Load("https://hello.world");
var node = document.DocumentNode.SelectSingleNode("//head/title");
Console.WriteLine("Page Title: " + node.InnerHtml);
```
This loads the HTML from the URL "https://hello.world", selects the part of the HTML corresponding to the headline title (`//head/title`), then prints it out.

## Deep Dive

Years back, C# developers had to rely on in-built .NET libraries like Html Agility Pack. This tool was handy but had limitations, like high memory usage when dealing with large documents. 

Today, alternatives have appeared. For example, AngleSharp paints a modern touch, with better performance and a more user-friendly API. It mimics the JavaScript DOM while adding LINQ capabilities, and it can even interpret CSS selectors. 

```C#
using AngleSharp;
...
var config = Configuration.Default.WithDefaultLoader();
var context = BrowsingContext.New(config);
var document = await context.OpenAsync("https://hello.world");
var cellData = document.QuerySelector("div.example");
Console.WriteLine(cellData.TextContent);
```
Remember, parsing HTML as a regular expression may seem easier but introduces complexity and is generally discouraged.

## See Also:

[Official Html Agility Pack Documentation](https://html-agility-pack.net/documentation)

[Official AngleSharp Documentation](https://anglesharp.github.io/docs/)

[When Not to Parse HTML with Regex](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454)