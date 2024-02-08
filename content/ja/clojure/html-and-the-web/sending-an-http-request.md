---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T17:59:45.465882-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送るとは、サーバーにデータを問い合わせたり操作を依頼したりすることです。プログラマーは通信のやり取りでWebサイトやAPIとやりとりするためにこれを行います。

## How to: (やり方)
ClojureでHTTPリクエストを送る基本的な方法です。`clj-http`ライブラリを使ってGETリクエストを示します。

```clojure
(require '[clj-http.client :as client])

;; GETリクエスト
(def response (client/get "https://api.example.com/data"))

;; レスポンスのステータスコードを出力
(println (:status response))

;; レスポンスボディを出力
(println (:body response))
```

出力例:
```
200
{"key1":"value1","key2":"value2"}
```

## Deep Dive (深掘り)
`clj-http`はClojureのHTTPクライアントライブラリで、Apache HttpClientのラッパーです。1999年に定義されたHTTP/1.1までとは異なり、現代のHTTPクライアントは非同期や新しいHTTP/2規格をサポートしています。`clj-http`は同期通信に重点を置いていますが、非同期リクエストもできます。代わりとして、`http-kit`や`aleph`も人気です。これらは並行性に強く、非同期通信にも対応しています。

実装時、以下の点を考慮する必要があります:
- エラーハンドリング: レスポンスが期待通りでない場合の対応が必要です。
- ヘッダーとパラメータ: 正確に指定しなければ、サーバーとの通信がうまくいかないことがあります。
- タイムアウト: 長時間のリクエストはタイムアウトを設定してリソースを守るべきです。
- レートリミット: 多くのAPIはリクエストの頻度に制限を設けています。リクエスト間隔を守らないとブロックされる可能性があります。

## See Also (関連リンク)
- `clj-http`リポジトリ: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure公式ドキュメント: [https://clojure.org/](https://clojure.org/)
- `http-kit`プロジェクト: [http://www.http-kit.org/](http://www.http-kit.org/)
- `aleph`ライブラリ: [https://github.com/ztellman/aleph](https://github.com/ztellman/aleph)
- Apache HttpClient: [https://hc.apache.org/httpcomponents-client-ga/](https://hc.apache.org/httpcomponents-client-ga/)
