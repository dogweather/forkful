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

# なぜ

HTTPリクエストを送信する理由について、ユーザーが取り組むメリットを最大限2文で説明します。

HTTPリクエストを送信することで、Webサーバーにデータをリクエストしたり、Webサーバーからデータを受け取ることができます。これにより、Webアプリケーションを作成したり、外部アプリケーションと連携させることができます。

# 使い方

下の「```Clojure ... ```」のコードブロックには、HTTPリクエストを送信するコーディング例と出力結果が含まれます。

```Clojure

; HTTPリクエストを送信するためのライブラリを取得
(require '[clj-http.client :as client])

; GETメソッドを使用して、Googleのトップページにリクエストを送信する
(client/get "http://www.google.com")

; 同じリクエストを送信し、レスポンスのbodyを受け取る
(def response (client/get "http://www.google.com"))
(:body response) ; => "<!doctype html><html>...</html>"

; POSTメソッドを使用して、POSTパラメーターをサーバーに送信する
(client/post "http://www.example.com" {:form-params {:username "johndoe" :password "123456"}})

```

# ディープダイブ

HTTPリクエストを送信するという行為は、単純にURLにアクセスすることではありません。実際には、リクエストの種類やパラメーター、ヘッダーなど、さまざまなオプションをつけることが可能です。また、HTTPリクエストのレスポンスを受け取る際にも、レスポンスのbodyのみを取得するのか、ヘッダーも含めて取得するのかなど、様々な操作が可能です。

さらに、Clojureでは高レベルのライブラリを使用するだけでなく、Javaの標準ライブラリを使用することもできます。そのため、より細かい設定やカスタマイズが必要な場合には、Javaの豊富なライブラリを利用することもできます。

# See Also

ここでは、HTTPリクエストを送信する際に役立つ関連記事やドキュメントへのリンクを紹介します。

- [Clojure Cookbook - HTTP Clients](https://clojure-cookbook.com/http/clients.html)
- [Clojure HTTP Client documentation](https://github.com/dakrone/clj-http)
- [Java HTTPURLConnection documentation](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)