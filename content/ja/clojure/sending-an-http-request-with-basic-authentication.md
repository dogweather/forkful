---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？
HTTPリクエストと基本認証（Basic Authentication）を行うとは、特定の情報にアクセスするために求められるIDとパスワードを用いて、サーバーにリクエストを送信することです。これにより、プログラマーは安全に信頼できるデータにアクセスし、そのデータを操作できます。

## どうやって：
```clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://example.com" {:basic-auth ["username" "password"]})]
  (println (:status response))
  (println (:body response)))
```
このサンプルコードですと、"http://example.com"にHTTPリクエストを送信し、その結果を表示します。"username"と"password"は、適切なユーザー名とパスワードに置き換えてください。

## ディープダイブ：
HTTPと基本認証はウェブの歴史と共に長年使用されてきました。しかし、現代ではより安全な認証方法、特にトークンベースの認証（例：OAuth2）が推奨されています。基本認証の場合、不正なリクエストを適切にブロックするには、SSL/TLSと組み合わせることが一般的です。

Clojureにおけるクライアント側のHTTPリクエストには`clj-http`パッケージがよく使われいます。このライブラリは、Javaの`Apache HttpClient`を基に作られています。

## 参照：
1. `clj-http`リポジトリ: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
2. ClojureにおけるHTTPリクエストの詳細なチュートリアル: [http://clojure-cookbook.com/](http://clojure-cookbook.com/)
3. HTTP Basic Authenticationについての詳細: [https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
4. OAuth2についての詳細: [https://oauth.net/2/](https://oauth.net/2/)