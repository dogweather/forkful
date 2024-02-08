---
title:                "HTMLの解析"
aliases:
- ja/c-sharp/parsing-html.md
date:                  2024-02-03T19:11:53.566745-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となく理由？

プログラミングにおけるHTMLの解析とは、HTMLドキュメントの構造を分析し、その内容をプログラムで抽出、操作、かつ対話することを可能にします。プログラマーは、ウェブスクレイピング、データの抽出、あるいはさまざまなアプリケーション用にウェブページやHTMLドキュメントを動的に変更するためにこれを行います。これはウェブ開発、データ分析、自動化テストシナリオにおいて重要なスキルとなります。

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
