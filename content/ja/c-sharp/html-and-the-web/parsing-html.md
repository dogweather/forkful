---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:53.566745-07:00
description: "\u65B9\u6CD5\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.119922-06:00'
model: gpt-4-0125-preview
summary: ".NET\u306F`HttpClient`\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30D5\
  \u30A7\u30C3\u30C1\u3059\u308B\u306A\u3069\u3001HTML\u4F5C\u696D\u306E\u305F\u3081\
  \u306E\u57FA\u672C\u7684\u306A\u30B5\u30DD\u30FC\u30C8\u3092\u63D0\u4F9B\u3057\u307E\
  \u3059\u304C\u3001\u7D44\u307F\u8FBC\u307F\u306E\u7DCF\u5408\u7684\u306AHTML\u30D1\
  \u30FC\u30B5\u30FC\u3092\u6B20\u3044\u3066\u3044\u307E\u3059\u3002\u305D\u306E\u305F\
  \u3081\u3001\u307B\u3068\u3093\u3069\u306EC#\u958B\u767A\u8005\u306F\u3001HtmlAgilityPack\u3084\
  AngleSharp\u306E\u3088\u3046\u306A\u4EBA\u6C17\u306E\u7B2C\u4E09\u8005\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306B\u983C\u308A\u307E\u3059\u3002\u3053\u308C\u3089\u306E\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u306FHTMLDOM\u306E\u7C21\u5358\u306A\u30AF\u30A8\u30EA\u3001\
  \u64CD\u4F5C\u3001\u304A\u3088\u3073\u30C8\u30E9\u30D0\u30FC\u30B5\u30EB\u3092\u53EF\
  \u80FD\u306B\u3057\u307E\u3059."
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
