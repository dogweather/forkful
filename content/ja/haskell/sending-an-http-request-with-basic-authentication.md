---
title:                "基本認証を用いた http リクエストの送信"
html_title:           "Haskell: 基本認証を用いた http リクエストの送信"
simple_title:         "基本認証を用いた http リクエストの送信"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

HaskellでのHTTPリクエストを基本認証で送信する方法

## なぜ

HTTPリクエストは、Webサーバーから情報を取得する方法の一つです。基本認証を使用することで、アカウントの認証を必要とする保護されたWebサイトへのアクセスが可能になります。

## 使い方

```Haskell
import Network.HTTP.Base (simpleHTTP, getRequest, setRequestBasicAuth)

main = do
  request <- getRequest "https://example.com"
  let requestWithAuth = setRequestBasicAuth "username" "password" request
  response <- simpleHTTP requestWithAuth
  putStrLn $ "Status code: " ++ show (rspCode response)
```

上記の例では、`Network.HTTP.Base`モジュールから`simpleHTTP`、`getRequest`、`setRequestBasicAuth`をインポートし、基本認証を使用したリクエストを送信しています。リクエストオブジェクトを作成し、`setRequestBasicAuth`を使用してユーザー名とパスワードを設定し、`simpleHTTP`を使用してサーバーにリクエストを送信します。最後に、HTTPレスポンスのステータスコードを出力します。

## 詳細を調べる

基本認証を使用するには、`setRequestBasicAuth`関数を使用してリクエストオブジェクトにユーザー名とパスワードを設定する必要があります。また、リクエストを送信する前にHTTPSであることを確認する必要があります。これには、`getRequest`関数を使用してリクエストオブジェクトを作成する際に、`"https://"`をURLの先頭に追加することで行うことができます。また、同じホストで複数のリクエストを送信する場合は、`setRequestBasicAuth`よりも`setRequestProxyAuth`関数を使用することをお勧めします。

## 他にも見る

- [Haskell.org](https://www.haskell.org/) - Haskellの公式サイト
- [Network.HTTP](https://hackage.haskell.org/package/HTTP) - HTTPリクエストを送信するためのHaskellライブラリ