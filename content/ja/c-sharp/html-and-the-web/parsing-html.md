---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:53.566745-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.119922-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308BHTML\u306E\
  \u89E3\u6790\u3068\u306F\u3001HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u69CB\
  \u9020\u3092\u5206\u6790\u3057\u3001\u305D\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u3067\u62BD\u51FA\u3001\u64CD\u4F5C\u3001\u304B\u3064\u5BFE\u8A71\u3059\
  \u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\
  \u30B0\u3001\u30C7\u30FC\u30BF\u306E\u62BD\u51FA\u3001\u3042\u308B\u3044\u306F\u3055\
  \u307E\u3056\u307E\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u7528\u306B\
  \u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3084HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\
  \u3092\u52D5\u7684\u306B\u5909\u66F4\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u30A6\u30A7\u30D6\u958B\u767A\u3001\
  \u30C7\u30FC\u30BF\u5206\u6790\u3001\u81EA\u52D5\u5316\u30C6\u30B9\u30C8\u30B7\u30CA\
  \u30EA\u30AA\u306B\u304A\u3044\u3066\u91CD\u8981\u306A\u30B9\u30AD\u30EB\u3068\u306A\
  \u308A\u307E\u3059\u3002."
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
