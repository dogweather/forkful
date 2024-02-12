---
title:                "解析HTML"
aliases: - /zh/c-sharp/parsing-html.md
date:                  2024-02-03T19:11:52.525213-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何为“解析HTML”以及为何需要？

在编程中解析HTML涉及到分析HTML文档的结构，允许您以编程方式提取、操作和与其内容交互。程序员这样做是为了自动化网页抓取、数据提取，甚至动态修改网页或HTML文档以适应各种应用程序，这使得它成为网站开发、数据分析和自动化测试场景中的一个基本技能。

## 如何操作：

虽然.NET为操作HTML提供了基本支持，比如用于获取网页的`HttpClient`，但它缺乏内置的、全面的HTML解析器。因此，大多数C#开发者转向像HtmlAgilityPack或AngleSharp这样的流行第三方库，以获得强大的HTML解析能力。这两个库都允许轻松地查询、操作和遍历HTML DOM。

### 使用HtmlAgilityPack

1. **安装HtmlAgilityPack**：首先，通过NuGet将HtmlAgilityPack包添加到您的项目中。
   ```
   Install-Package HtmlAgilityPack
   ```

2. **示例代码**：解析一个HTML字符串，并提取所有`<h1>`元素的标题。

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
                             <h1>标题 1</h1>
                             <h1>标题 2</h1>
                         </body>
                        </html>";
           var htmlDoc = new HtmlDocument();
           htmlDoc.LoadHtml(html);

           var h1标签 = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var title in h1标签)
           {
               Console.WriteLine(title);
           }
       }
   }
   ```

   **示例输出：**
   ```
   标题 1
   标题 2
   ```

### 使用AngleSharp

1. **安装AngleSharp**：通过NuGet将AngleSharp库添加到您的项目中。
   ```
   Install-Package AngleSharp
   ```

2. **示例代码**：加载一个HTML文档并查询具有特定类的`div`元素。

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
           var document = await context.OpenAsync(req => req.Content("<div class='item'>项目 1</div><div class='item'>项目 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **示例输出：**
   ```
   项目 1
   项目 2
   ```

HtmlAgilityPack和AngleSharp都是强大的HTML解析工具，但在两者之间的选择可能取决于特定项目需求、性能考虑或个人对API设计的偏好。
