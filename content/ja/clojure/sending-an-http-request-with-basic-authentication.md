---
title:                "基本認証を使用したhttpリクエストの送信"
html_title:           "Clojure: 基本認証を使用したhttpリクエストの送信"
simple_title:         "基本認証を使用したhttpリクエストの送信"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 何をするのか & なぜするのか?
HTTPリクエストを基本認証で送るとは、ウェブサーバーに保護されたリソースへアクセスするためにユーザーが認証情報を提供することを意味します。プログラマーは基本認証を使用することで、セキュリティ保護されたリソースへのアクセスを制限することができます。

## 方法:
```Clojure
;; 使用するライブラリをインポート
(require '[clj-http.client :as client])

;; リクエストを送信する関数を定義
(defn send-request [url username password]
  ;; 基本認証用のヘッダーを設定
  (let [auth-header (str "Basic " (b64/encode (str username ":" password)))]
    ;; リクエストを送信
    (client/get url {:headers { "Authorization" auth-header }})))

;; リクエストを送信し、レスポンスを受け取る
(def response (send-request "http://example.com" "username" "password"))

;; レスポンスからステータスコードを取得
(:status response)

;; レスポンスからボディを取得
(:body response)
```

## 深堀り:
- 基本認証は、HTTPプロトコルの基本的なセキュリティ認証方法の1つです。
- 代替として、OAuthやOpenID Connectなどのよりセキュアな認証方法があります。
- 基本認証は、リクエストヘッダーにユーザー名とパスワードを含むことで実現されます。

## 関連情報を見る:
- [Clj-httpクライアントライブラリの公式ドキュメント](https://github.com/dakrone/clj-http)
- [HTTP基本認証の詳細な解説](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)