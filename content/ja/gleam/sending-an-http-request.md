---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何＆なぜ？
HTTPリクエストを送信するとは、サービーサイドに情報をリクエストするクライアントサイドの行動です。開発者はこれを使用して、APIからデータを取得したり、リモートサーバーとの間でデータを交換したりします。

## 実行方法：
以下に、GleamでHTTPリクエストを送信するパターンのコード例を示します。

```Gleam
import gleam/httpc

let response =
  httpc.get("https://api.example.com")
  |> httpc.send()

case response {
  Ok(response) ->
    io.println(response.body)
  Error(e) ->
    io.println("HTTP request failed with", e)
}
```
このコード実行結果は、リクエストに成功した場合はAPIからの応答を表示し、失敗した場合はエラーメッセージを表示します。

## 深掘り：
1. **歴史的文脈**：HTTPリクエストの送信は、Webが成立した初期から存在しています。これはサーバとクライアント間の情報交換の基礎です。
2. **代替手段**：他の代替手段としては、WebSocketsやServer-Sent Eventsなどがあります。これらはリアルタイムのデータフローを可能にしますが、HTTPリクエストに比べて複雑さが増します。
3. **実装詳細**：GleamでのHTTPリクエストの送信は、`httpc`ライブラリを使用します。これは非同期性とエラーハンドリングを標準で提供します。

## 参考リンク
GleamでHTTPリクエストを深く探求したい方は以下のリンクをチェックしてください。