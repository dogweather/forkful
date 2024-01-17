---
title:                "ウェブページのダウンロード"
html_title:           "C#: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何を & なぜ？
Webページのダウンロードとは、ウェブサイトからコンテンツを取得することです。プログラマーがWebページをダウンロードする理由は、そのページから情報を収集して、処理や分析を行うことです。

## 方法：
```C#
var url = "https://example.com";
var client = new WebClient();
var html = client.DownloadString(url);
Console.WriteLine(html);
```

## 深く掘り下げる：
Webページのダウンロードは、インターネットが普及する以前から行われてきました。代替手段として、WebスクレイピングやAPIを使用することもできます。C#では、WebClientやHttpClientなどのクラスを使用してWebページのダウンロードを実装することができます。また、ユーザーエージェントやプロキシの設定なども可能です。

## 関連情報：
- [WebClientクラスドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [HttpClientクラスドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [C#でWebスクレイピングを行う方法](https://qiita.com/zaburo/items/1723427f64882fcbbe98)
- [C#でAPIを使用する方法](https://csharp.keicode.com/basics/web-api.php)