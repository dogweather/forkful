---
title:                "HTTPリクエストを送信する"
html_title:           "Gleam: HTTPリクエストを送信する"
simple_title:         "HTTPリクエストを送信する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ
ほとんどのwebアプリケーションは、HTTPリクエストを送信することでデータを取得したり送信したりすることに依存しています。Gleamでは、HTTPリクエストを送信することで、外部のAPIやサーバーとやり取りをすることができます。

## 方法
まず、GleamのHTTPモジュールをインポートします。
```
Gleamモジュール

import gleam/http
```

次に、送信したいHTTPリクエストの設定を行います。たとえば、GoogleのAPIからJSON形式のデータを取得する場合、以下のようにします。
```
リクエストを設定

let request = http.request(
    method: "GET",
    url: "https://www.googleapis.com/books/v1/volumes",
    headers: [(key: "Accept", value: "application/json")],
    body: some(gleam/json/encode({"q": "Gleam programming"}))
)
```

設定したリクエストを送信し、結果を取得します。
```
リクエストを送信

let response = http.send(request)

```

最後に、結果を確認し、必要なデータを取得します。
```
結果を確認

case response {
    Ok(resp) -> resp.body
    Err(_err) -> HttpError
}
```

## 深堀り
リクエストを送信する際、さまざまなオプションを設定することができます。たとえば、ヘッダーやクエリーパラメーターの指定、Basic認証の追加などが可能です。詳細な設定方法については、Gleamの公式ドキュメントを参照してください。

## さらに見る
- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [GleamのHTTPモジュールについて](https://gleam.run/documentation/#http)