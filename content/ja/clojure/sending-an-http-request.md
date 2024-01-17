---
title:                "HTTPリクエストの送信"
html_title:           "Clojure: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

HTTPリクエストを送るとは、ウェブサーバーに対してデータを要求することです。プログラマーがHTTPリクエストを送る理由は、ネットワーク通信を行うことによって、アプリケーションやウェブサイトで必要なデータを取得するためです。

## 方法：

### GETリクエストを送る

```Clojure
(http/get "https://example.com")
```

### POSTリクエストを送る

```Clojure
(http/post "https://example.com" {:body "some data"})
```

### サーバーからのレスポンスを取得する

```Clojure
(def response (http/get "https://example.com"))

(str "Response status: " (:status response))
;=> "Response status: 200"
```

## 深堀り：

### 送信方法の歴史的背景

HTTPリクエストは、ウェブの発展に伴って広く使用されるようになりました。最初のバージョンであるHTTP/1.0は1996年に発表され、その後改定されました。現在のバージョンはHTTP/2.0です。

### 代替手段

HTTPリクエストには、他にも多くの言語やツールで実装されたライブラリがあります。Clojure以外にも、PythonやRubyなどのプログラミング言語で使用することができます。

### 実装の詳細

Clojureでは、HTTPリクエストを送信するために「clj-http」ライブラリを使用することができます。このライブラリは、HTTPクライアントとして動作するClojureの関数を提供します。

## 関連リンク：

- [ClojureでHTTPリクエストを送信する方法](https://clojure.org/guides/http_clients)
- [HTTP/1.0の仕様書](https://www.rfc-editor.org/rfc/rfc1945)
- [HTTP/2.0の仕様書](https://www.rfc-editor.org/rfc/rfc7540)