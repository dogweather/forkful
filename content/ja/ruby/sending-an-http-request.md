---
title:                "Ruby: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ
HTTPリクエストを送信することの *なぜ* が重要なのかを説明します。HTTPリクエストとは、Webサーバーに対してリクエストを送信し、Webページやデータを取得するためのものです。

## はじめる前に
RubyでHTTPリクエストを送信する方法を解説します。以下のコードを参考にしてください。

```Ruby
require 'net/http'
url = URI("https://www.example.com")
response = Net::HTTP.get(url)
puts response
```

このコードでは、 `net/http` ライブラリを使用して、 `www.example.com` にHTTPリクエストを送信し、そのレスポンスを取得しています。`puts` 関数を使用することで、レスポンスをターミナルに表示することができます。

## 深く掘り下げる
さらに深く、HTTPリクエストを送信する方法について掘り下げていきましょう。HTTPリクエストには、GETリクエストとPOSTリクエストの2種類があります。GETリクエストは、URLにパラメーターを付加してデータを送信するために使用されます。一方、POSTリクエストは、HTTPヘッダーとメッセージボディを使用してデータを送信するために使用されます。

さらに、HTTPリクエストを送信する際には、レスポンスのステータスコードを確認することも重要です。ステータスコードは、リクエストが成功したかどうかや、エラーが発生したかどうかを示します。

## 参考リンク
こちらのリンクから、より詳しい情報を入手することができます。

- [Ruby公式ドキュメント](https://www.ruby-lang.org/ja/documentation/)
- [HTTPリクエストの仕組み](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [Net::HTTPライブラリ](https://docs.ruby-lang.org/ja/latest/library/net=2fhttp.html)
- [ステータスコード一覧](https://developer.mozilla.org/ja/docs/Web/HTTP/Status)