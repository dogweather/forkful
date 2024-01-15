---
title:                "基本認証を使用したHTTPリクエストの送信"
html_title:           "Clojure: 基本認証を使用したHTTPリクエストの送信"
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

あなたは、ネットワーク上での安全な通信を確保するために、クライアントが身元を証明できるようにするために、HTTPリクエストに基本認証を設定したいかもしれません。この記事では、ClojureでHTTPリクエストを送信する方法を学ぶことができます。

## How To

まずは、Clojureのライブラリであるclj-httpをインストールしましょう。

```Clojure
(require '[clj-http.client :as client])
```

次に、基本認証を行うために、`:basic-auth`キーワードを利用します。これには、ユーザー名とパスワードを含むマップが必要です。

```Clojure
(client/get "https://example.com"
            {:basic-auth {:user "username" :password "password"}})
```

これで、基本認証を行いながら、HTTPSリクエストを送信することができます。応答は、クロージャのマップとして返ってきます。

```Clojure
{:status 200
 :headers {"Content-Type" "text/html"}
 :body "<html>...</html>"}
```

基本認証を行わずにHTTPSリクエストを送信する場合は、`client/get`の第二引数を`nil`に設定します。

## Deep Dive

clj-httpは、ClojureでHTTPリクエストを送信するための便利なライブラリです。しかし、基本認証に限らず、様々なカスタマイズやオプションがあります。例えば、ヘッダーを指定したり、クッキーを付け加えたりすることもできます。

また、基本的なGETリクエストの他にも、POSTやPUTなどのメソッドを指定することもできます。

## See Also

- [clj-http ドキュメント](https://github.com/dakrone/clj-http)
- [Clojure 公式サイト](https://clojure.org/)
- [HTTP リクエスト基本認証についての詳細](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)