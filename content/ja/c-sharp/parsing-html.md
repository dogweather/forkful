---
title:                "HTMLの解析"
html_title:           "C#: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## なぜHTMLの解析に取り組むのか

HTMLはWeb上で最も一般的に使用される言語の1つであり、私たちが見ているほとんどのウェブページはHTMLコードから構築されています。そのため、HTMLを解析することは非常に有用です。例えば、ウェブスクレイピングやデータ抽出など、さまざまなタスクを行う際に役立ちます。

## 方法

HTMLを解析するには、C#で使用できるいくつかのライブラリがあります。ここでは、HtmlAgilityPackというライブラリを使ってみましょう。

まず、プロジェクトにHtmlAgilityPackをインストールします。次に、解析するHTMLコードを```<html></html>```タグで囲んで、```HtmlDocument```オブジェクトを使用してコードを読み込みます。

```
var htmlCode = @"<html>
<head>
<title>Hello, world!</title>
</head>
<body>
<h1>Hello</h1>
<p>This is a sample paragraph.</p>
</body>
</html>";

var document = new HtmlAgilityPack.HtmlDocument();
document.LoadHtml(htmlCode);
```

これで、HTMLコードを解析する準備が整いました。例えば、```SelectSingleNode```メソッドを使用して、特定のタグを取得することができます。

```
var title = document.DocumentNode.SelectSingleNode("//title").InnerText;
Console.WriteLine(title);
```

出力結果は、```Hello, world!```となります。

## 詳細

HtmlAgilityPackを使用すると、より複雑なHTMLコードを解析することもできます。例えば、```SelectNodes```メソッドを使用して、複数のタグを取得することも可能です。また、```XPath```パスを指定して、より具体的なタグを取得することもできます。

さらに、HtmlAgilityPackには、HTMLを書き出す機能もあります。例えば、```HtmlDocument```オブジェクトの```Save```メソッドを使用して、解析したHTMLを別のファイルに保存することができます。

## 関連リンク

- [HtmlAgilityPackドキュメント](https://html-agility-pack.net/documentation)
- [C#でHTMLを解析する方法 - Qiita](https://qiita.com/satoshi1213/items/bf54596d439041d3bc8e)
- [C#でHTMLを書き出す方法 - Stack Overflow](https://stackoverflow.com/questions/6649813/how-to-write-a-html-document-using-c-sharp-net)
- [ReactでHTMLを解析する方法 - TechAcademyマガジン](https://techacademy.jp/magazine/17852)