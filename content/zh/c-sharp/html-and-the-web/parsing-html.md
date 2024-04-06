---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:52.525213-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A 1. **\u5B89\u88C5HtmlAgilityPack**\uFF1A\
  \u9996\u5148\uFF0C\u901A\u8FC7NuGet\u5C06HtmlAgilityPack\u5305\u6DFB\u52A0\u5230\
  \u60A8\u7684\u9879\u76EE\u4E2D\u3002"
lastmod: '2024-04-05T22:38:46.927946-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u867D\u7136.NET\u4E3A\u64CD\u4F5CHTML\u63D0\
  \u4F9B\u4E86\u57FA\u672C\u652F\u6301\uFF0C\u6BD4\u5982\u7528\u4E8E\u83B7\u53D6\u7F51\
  \u9875\u7684`HttpClient`\uFF0C\u4F46\u5B83\u7F3A\u4E4F\u5185\u7F6E\u7684\u3001\u5168\
  \u9762\u7684HTML\u89E3\u6790\u5668\u3002\u56E0\u6B64\uFF0C\u5927\u591A\u6570C#\u5F00\
  \u53D1\u8005\u8F6C\u5411\u50CFHtmlAgilityPack\u6216AngleSharp\u8FD9\u6837\u7684\u6D41\
  \u884C\u7B2C\u4E09\u65B9\u5E93\uFF0C\u4EE5\u83B7\u5F97\u5F3A\u5927\u7684HTML\u89E3\
  \u6790\u80FD\u529B\u3002\u8FD9\u4E24\u4E2A\u5E93\u90FD\u5141\u8BB8\u8F7B\u677E\u5730\
  \u67E5\u8BE2\u3001\u64CD\u4F5C\u548C\u904D\u5386HTML DOM\u3002"
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
