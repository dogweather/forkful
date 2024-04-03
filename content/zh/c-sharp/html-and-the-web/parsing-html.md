---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:52.525213-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u5230\u5206\u6790\
  HTML\u6587\u6863\u7684\u7ED3\u6784\uFF0C\u5141\u8BB8\u60A8\u4EE5\u7F16\u7A0B\u65B9\
  \u5F0F\u63D0\u53D6\u3001\u64CD\u4F5C\u548C\u4E0E\u5176\u5185\u5BB9\u4EA4\u4E92\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\u7F51\u9875\
  \u6293\u53D6\u3001\u6570\u636E\u63D0\u53D6\uFF0C\u751A\u81F3\u52A8\u6001\u4FEE\u6539\
  \u7F51\u9875\u6216HTML\u6587\u6863\u4EE5\u9002\u5E94\u5404\u79CD\u5E94\u7528\u7A0B\
  \u5E8F\uFF0C\u8FD9\u4F7F\u5F97\u5B83\u6210\u4E3A\u7F51\u7AD9\u5F00\u53D1\u3001\u6570\
  \u636E\u5206\u6790\u548C\u81EA\u52A8\u5316\u6D4B\u8BD5\u573A\u666F\u4E2D\u7684\u4E00\
  \u4E2A\u57FA\u672C\u6280\u80FD\u3002"
lastmod: '2024-03-13T22:44:47.765241-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u5230\u5206\u6790HTML\u6587\
  \u6863\u7684\u7ED3\u6784\uFF0C\u5141\u8BB8\u60A8\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u63D0\
  \u53D6\u3001\u64CD\u4F5C\u548C\u4E0E\u5176\u5185\u5BB9\u4EA4\u4E92\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u81EA\u52A8\u5316\u7F51\u9875\u6293\u53D6\
  \u3001\u6570\u636E\u63D0\u53D6\uFF0C\u751A\u81F3\u52A8\u6001\u4FEE\u6539\u7F51\u9875\
  \u6216HTML\u6587\u6863\u4EE5\u9002\u5E94\u5404\u79CD\u5E94\u7528\u7A0B\u5E8F\uFF0C\
  \u8FD9\u4F7F\u5F97\u5B83\u6210\u4E3A\u7F51\u7AD9\u5F00\u53D1\u3001\u6570\u636E\u5206\
  \u6790\u548C\u81EA\u52A8\u5316\u6D4B\u8BD5\u573A\u666F\u4E2D\u7684\u4E00\u4E2A\u57FA\
  \u672C\u6280\u80FD\u3002."
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
