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

## なぜ
ウェブページをダウンロードすることのメリットは、オフラインでもウェブページを閲覧できることです。また、様々なデータを収集するためにも使用できます。

## 方法
```C#
using System;
using System.Net;

string url = "https://www.example.com";

// URLからドキュメントを取得する
WebClient client = new WebClient();
string html = client.DownloadString(url);

Console.WriteLine(html); // ウェブページのHTMLコードを出力
```

上記のコードを実行すると、指定したURLからHTMLコードをダウンロードできます。また、ダウンロードする際にはセキュリティ証明書の有無などを確認できます。

## 詳細
ウェブページのダウンロードでは、HTTPリクエストを使用してサーバーにアクセスし、HTMLコードを取得します。その際、ヘッダー情報を使用してセッションや言語の設定なども行うことができます。また、リクエストのボディにはデータを含めることもできます。

## 参考リンク
- [WebClient.DownloadString メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.webclient.downloadstring)
- [HTTPリクエストの詳細](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)