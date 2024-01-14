---
title:                "C#: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

こんにちは、プログラマーのみなさん。今日は、Webページをダウンロードすることがどのようにできるかについてお話しします。

## Why

Webページをダウンロードすることがなぜ重要なのかというと、自分のプログラムやアプリケーションに必要な情報を簡単に取得することができるからです。インターネット上には様々なデータやコンテンツが溢れており、それらをダウンロードすることで自分のプログラムをより有用なものにすることができます。

## How To

まずは、Webページをダウンロードするためにはどのようなツールやメソッドが必要かを見ていきましょう。C#にはこれを簡単に実現できる```WebClient```というクラスがあります。これを使用することで、URLを指定してWebページをダウンロードすることができます。

```C#
WebClient client = new WebClient();
string htmlCode = client.DownloadString("https://www.example.com");
Console.WriteLine(htmlCode);
```
実行すると、指定したURLのWebページのHTMLコードがダウンロードされてコンソール画面に表示されます。これで必要な情報を取得することができます。

## Deep Dive

さらに深くWebページをダウンロードする方法を見てみましょう。上記の方法ではHTMLコードを取得することができますが、特定の要素や属性を取得したい場合は```HtmlAgilityPack```というライブラリを使用すると便利です。これを使うことで、HTMLコードを解析して特定の要素を取得することができます。

また、ダウンロードしたWebページをローカルに保存したい場合は```WebClient```の```DownloadFile```メソッドを使用することができます。これを使用することで、指定したURLのWebページを指定したパスにダウンロードすることができます。

## See Also

- [Microsoft Docs: WebClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient)
- [HtmlAgilityPack: HTML Parser in C#](https://html-agility-pack.net/)
- [C#でWebページをダウンロードする方法](https://codeday.me/jp/qa/20190517/228541.html)