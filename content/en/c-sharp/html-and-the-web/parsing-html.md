---
title:                "Parsing HTML"
aliases:
- /en/c-sharp/parsing-html.md
date:                  2024-02-03T19:02:47.855631-07:00
model:                 gpt-4-0125-preview
simple_title:         "Parsing HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML in programming involves analyzing the structure of an HTML document, allowing you to extract, manipulate, and interact with its content programmatically. Programmers do this to automate web scraping, data extraction, or even modify web pages or HTML documents dynamically for various applications, making it an essential skill in web development, data analysis, and automated testing scenarios.

## How to:

While .NET provides basic support for working with HTML, such as the `HttpClient` for fetching web pages, it lacks a built-in, comprehensive HTML parser. Therefore, most C# developers turn to popular third-party libraries like HtmlAgilityPack or AngleSharp for robust HTML parsing capabilities. Both libraries allow easy querying, manipulation, and traversal of the HTML DOM.

### Using HtmlAgilityPack

1. **Install HtmlAgilityPack**: First, add the HtmlAgilityPack package to your project via NuGet.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **Example Code**: Parse an HTML string, and extract the titles of all `<h1>` elements.

   ```csharp
   using HtmlAgilityPack;
   using System;
   using System.Linq;

   class Program
   {
       static void Main(string[] args)
       {
           var html = @"<html>
                         <body>
                             <h1>Title 1</h1>
                             <h1>Title 2</h1>
                         </body>
                        </html>";
           var htmlDoc = new HtmlDocument();
           htmlDoc.LoadHtml(html);

           var h1Tags = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var title in h1Tags)
           {
               Console.WriteLine(title);
           }
       }
   }
   ```

   **Sample Output:**
   ```
   Title 1
   Title 2
   ```

### Using AngleSharp

1. **Install AngleSharp**: Add the AngleSharp library to your project through NuGet.
   ```
   Install-Package AngleSharp
   ```

2. **Example Code**: Load an HTML document and query `div` elements with a specific class.

   ```csharp
   using AngleSharp;
   using AngleSharp.Dom;
   using System;
   using System.Linq;
   using System.Threading.Tasks;

   class Program
   {
       static async Task Main(string[] args)
       {
           var context = BrowsingContext.New(Configuration.Default);
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Item 1</div><div class='item'>Item 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **Sample Output:**
   ```
   Item 1
   Item 2
   ```

Both HTMLAgilityPack and AngleSharp are powerful tools for parsing HTML, but your choice between them might depend on specific project requirements, performance considerations, or personal preference in API design.
