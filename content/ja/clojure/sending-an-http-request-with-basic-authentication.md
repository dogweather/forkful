---
title:                "Clojure: 「基本認証付きでhttpリクエストを送信する」"
simple_title:         "「基本認証付きでhttpリクエストを送信する」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信する際に基本認証を使う理由を説明します。

基本認証は、ユーザー名とパスワードを使用して安全にデータを送信するための一般的な方法です。この方法を使用することで、ユーザーは自分のアカウントを保護し、機密情報を安全に送信することができます。

## 方法

基本認証を使用してHTTPリクエストを送信する方法を見ていきましょう。まずは、依存関係を設定する必要があります。

```Clojure
(require '[clj-http.client :as http])
```

次に、リクエストを送信するためのURLと認証情報を設定します。

```Clojure
(def url "https://example.com")
(def auth {:basic-creds "username" "password"})
```

最後に、実際にリクエストを送信し、結果を受け取ります。

```Clojure
(def result (http/get url {:auth auth}))
(println (:status result)) ; 200 (成功)
(println (:body result)) ; リクエストの結果が表示されます
```

## ディープダイブ

基本認証は、HTTPリクエスト時に安全な認証情報を送信するための重要な手段です。認証情報を使用することで、データの安全性を確保し、不正なアクセスからアカウントを保護することができます。

さらに、Clojureの ```clj-http``` ライブラリを使用することで、認証情報を簡単に設定してリクエストを送信することができます。このライブラリは、ClojureでHTTPリクエストを扱うための便利なツールです。

## 関連リンク

- [HTTPリクエストを送信する方法 | Clojure Doc](https://clojuredocs.org/clojure.core/http-post)
- [基本認証 | MDN（Mozilla Developer Network）](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [clj-httpクライアント | GitHub](https://github.com/dakrone/clj-http)