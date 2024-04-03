---
date: 2024-01-20 17:59:45.465882-07:00
description: "How to: (\u3084\u308A\u65B9) Clojure\u3067HTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u3092\u9001\u308B\u57FA\u672C\u7684\u306A\u65B9\u6CD5\u3067\u3059\u3002`clj-http`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3063\u3066GET\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.553889-06:00'
model: gpt-4-1106-preview
summary: "Clojure\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u57FA\u672C\
  \u7684\u306A\u65B9\u6CD5\u3067\u3059\u3002`clj-http`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u4F7F\u3063\u3066GET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u793A\u3057\u307E\
  \u3059."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
