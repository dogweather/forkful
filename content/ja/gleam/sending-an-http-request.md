---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T17:59:38.419566-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
HTTPリクエストを送ることは、インターネット上で情報を取得したり、サーバにデータを送ったりする行為です。プログラマは、ウェブのリソースとやりとりするため、またAPIを使用してデータを操作するためにこれを行います。

## How to: (方法：)
```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn request_example() {
  // HTTP GETリクエストを送る
  case httpc.send(req: http.Request(to_url("https://example.com"))) {
    Ok(response) -> 
      // 成功した場合の処理
      io.println("Got a response: ")
      io.println(response)
    Error(error) ->
      // エラーが発生した場合の処理
      io.println("Something went wrong: ")
      io.println(error)
  }
}
```
サンプル出力：
```
Got a response: Response(200, "OK", [], "Hello, world!")
```

## Deep Dive (深い潜水)
HTTPリクエストの概念は1990年前後に誕生しました。プロトコル自体がインターネット通信の基礎となります。Gleamでは、標準的な`http`モジュールの他に、いくつかのHTTPクライアントライブラリーが利用可能です。直接`http`モジュールを使うか、あるいは`gleam_http`や`gleam_reqwest`のようなラッパーを使用して、HTTPリクエストの詳細を抽象化することができます。多様な要求に合致するため、同期的にも非同期的にもリクエストを送れます。さらに、Gleamは型安全なので、リクエストの作成と応答の処理におけるエラーをコンパイル時に捕捉することができます。

## See Also (関連情報)
- Gleamの公式文書: [https://gleam.run](https://gleam.run)
- HTTPに関するRFC: [https://tools.ietf.org/html/rfc2616](https://tools.ietf.org/html/rfc2616)
- Gleam http モジュール: [https://hexdocs.pm/gleam_http](https://hexdocs.pm/gleam_http)