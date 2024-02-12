---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases: - /ja/clojure/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:22.689767-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？

HTTPリクエストに基本認証を利用することは、ユーザー名とパスワードで保護されたリソースにアクセスする方法です。この技術はAPIやサービスに安全にアクセスするためによく使われます。

## How to:
## 方法：

```clojure
(require '[clj-http.client :as client])

(defn fetch-protected-resource [url username password]
  (let [credentials (str username ":" password)
        encoded-creds (clojure.core/base64-encode (.getBytes credentials))]
    (client/get url {:headers {"Authorization" (str "Basic " encoded-creds)}})))

;; 使用例:
(println (fetch-protected-resource "https://example.com/protected" "myuser" "mypassword"))
```

期待される出力：

```clojure
{:status 200, :headers { ... }, :body "..." }
```

## Deep Dive
## 詳細な情報：

基本認証は、HTTPプロトコルの古い形式の認証方法で、RFC 7617で定義されています。セキュリティが強固でないため、HTTPSと組み合わせて使用されることが一般的です。代替手段としては、OAuthやAPIキーなど、より安全な認証方式があります。

Clojureでは`clj-http`ライブラリが人気で、HTTPリクエストを手軽に行えます。上記のコードでは、ユーザー名とパスワードから"Authorization"ヘッダーを生成しています。`base64-encode`を利用し、エンコードされた認証情報をHTTPリクエストに持たせています。

## See Also
## 関連情報：

- HTTP基本認証 (RFC 7617): https://tools.ietf.org/html/rfc7617
- clj-httpドキュメント: https://github.com/dakrone/clj-http
- Clojure公式サイト: https://clojure.org
- セキュリティ強化のためのHTTPS: https://letsencrypt.org/
