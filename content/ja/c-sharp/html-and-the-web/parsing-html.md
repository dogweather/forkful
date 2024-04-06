---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:53.566745-07:00
description: "\u65B9\u6CD5\uFF1A 1. **HtmlAgilityPack\u306E\u30A4\u30F3\u30B9\u30C8\
  \u30FC\u30EB**: \u307E\u305A\u3001NuGet\u3092\u901A\u3058\u3066\u30D7\u30ED\u30B8\
  \u30A7\u30AF\u30C8\u306BHtmlAgilityPack\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u8FFD\
  \u52A0\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.995637-06:00'
model: gpt-4-0125-preview
summary: ''
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## 方法：
.NETは`HttpClient`でウェブページをフェッチするなど、HTML作業のための基本的なサポートを提供しますが、組み込みの総合的なHTMLパーサーを欠いています。そのため、ほとんどのC#開発者は、HtmlAgilityPackやAngleSharpのような人気の第三者ライブラリに頼ります。これらのライブラリはHTMLDOMの簡単なクエリ、操作、およびトラバーサルを可能にします。

### HtmlAgilityPackを使用する
1. **HtmlAgilityPackのインストール**: まず、NuGetを通じてプロジェクトにHtmlAgilityPackパッケージを追加します。
   ```
   Install-Package HtmlAgilityPack
   ```

2. **サンプルコード**: HTML文字列を解析し、すべての`<h1>`要素のタイトルを抽出します。

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

   **サンプル出力:**
   ```
   Title 1
   Title 2
   ```

### AngleSharpの使用
1. **AngleSharpのインストール**: NuGetを通じてプロジェクトにAngleSharpライブラリを追加します。
   ```
   Install-Package AngleSharp
   ```

2. **サンプルコード**: HTMLドキュメントを読み込み、特定のクラスを持つ`div`要素をクエリします。

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

   **サンプル出力:**
   ```
   Item 1
   Item 2
   ```

HTMLAgilityPackとAngleSharpはどちらもHTMLを解析するための強力なツールですが、特定のプロジェクト要件、パフォーマンスへの考慮、またはAPIデザインに対する個人的な好みに応じて選択が変わる場合があります。
