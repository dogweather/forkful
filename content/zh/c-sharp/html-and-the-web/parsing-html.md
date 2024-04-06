---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:52.525213-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A 1. **\u5B89\u88C5HtmlAgilityPack**\uFF1A\
  \u9996\u5148\uFF0C\u901A\u8FC7NuGet\u5C06HtmlAgilityPack\u5305\u6DFB\u52A0\u5230\
  \u60A8\u7684\u9879\u76EE\u4E2D\u3002"
lastmod: '2024-04-05T21:53:48.079578-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u89E3\u6790HTML"
weight: 43
---

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
