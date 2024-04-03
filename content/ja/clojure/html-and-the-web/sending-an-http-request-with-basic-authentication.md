---
date: 2024-01-20 18:01:22.689767-07:00
description: "\u4F55\u3068\u306A\u305C\uFF1F HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\
  \u57FA\u672C\u8A8D\u8A3C\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u306F\u3001\u30E6\
  \u30FC\u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3067\u4FDD\u8B77\u3055\
  \u308C\u305F\u30EA\u30BD\u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u65B9\
  \u6CD5\u3067\u3059\u3002\u3053\u306E\u6280\u8853\u306FAPI\u3084\u30B5\u30FC\u30D3\
  \u30B9\u306B\u5B89\u5168\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306B\
  \u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.557879-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u5229\
  \u7528\u3059\u308B\u3053\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\
  \u30B9\u30EF\u30FC\u30C9\u3067\u4FDD\u8B77\u3055\u308C\u305F\u30EA\u30BD\u30FC\u30B9\
  \u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u3053\u306E\
  \u6280\u8853\u306FAPI\u3084\u30B5\u30FC\u30D3\u30B9\u306B\u5B89\u5168\u306B\u30A2\
  \u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306B\u3088\u304F\u4F7F\u308F\u308C\u307E\
  \u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
