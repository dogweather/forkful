---
title:                "「HTTPリクエストの送信」"
html_title:           "Gleam: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# GleamでHTTPリクエストを送信する

## なに？なぜ？

HTTPリクエストを送信するとは、ウェブ上の情報を取得したりデータを更新したりするために、インターネット上のサーバーと通信することです。
プログラマーがHTTPリクエストを送信する理由は、ユーザーがウェブサイトにアクセスした時に、情報を取得したり送信したりするためです。例えば、あなたがウェブサイトにログインするとき、HTTPリクエストを送信して認証情報を送信し、ウェブサイトにアクセスできるようにします。

## 使い方：

Gleamでは、`Http`モジュールを使用することで簡単にHTTPリクエストを送信することができます。以下のコードは、GleamでGETリクエストを送信し、レスポンスを取得する例です。

```Gleam
import Http

let url = "https://example.com"

let response = Http.get(url)

case response {
  Ok(response) -> response.body
  Error(err) -> "Error!"
}
```
実行すると、`"Hello, World!"`のようなレスポンスが返ってきます。

POSTリクエストを送信する場合は、以下のように書くことができます。
```Gleam
import Http

let url = "https://example.com"
let body = """{"username": "John", "password": "secret"}"""

let response = Http.post(url, body)

case response {
  Ok(response) -> response.body
  Error(err) -> "Error!"
}
```

## 詳細を掘り下げる

### 歴史的な背景
HTTPリクエストは、ウェブの発展にとって非常に重要な役割を果たしてきました。1990年代に開発されたHTTPプロトコルは、ウェブブラウザとウェブサーバー間の通信を可能にしました。

### 代替手段
HTTPリクエストを送信するには、他にも様々な手段があります。例えば、CurlやNet/HTTPといったライブラリを使用する方法や、GoやPythonのような言語でHTTPリクエストをネイティブにサポートしている方法などがありますが、Gleamを使用することで型安全性や他の言語との相互運用性が向上します。

### 実装の詳細
Gleamの`Http`モジュールでは、内部的にはErlangの`httpc`モジュールを使用しています。また、`body`関数を使用することでレスポンスのボディーだけでなく、ヘッダーやステータスコードも取得することができます。

## 関連情報

- [Gleam Language Website](https://gleam.run/)
- [Official Guide: Making HTTP requests](https://gleam.run/book/integrating-with-other-projects#making-http-requests)
- [HTTP Request in Erlang](https://erlang.org/doc/man/httpc.html)