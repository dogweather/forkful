---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストに基本認証を使用するとは、サーバーに対して認証の情報と共にリクエストを送信することを指します。これは、認証が必要なWebリソースにアクセスするために行われます。

## 利用法

以下は、HTTPリクエストに基本認証を利用する基本的なHaskellコード例です：

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import Network.HTTP.Client () -- For Request instance

main :: IO ()
main = do
    initRequest <- parseRequest "http://example.com/"
    let request = applyBasicAuth "my_user" "my_password" initRequest
    response <- httpLBS request
    print response
```

これを実行すると、次のような出力が得られます：

```Haskell
Status code: 200
Headers: [("Server","nginx"), ("Content-Type","text/html; charset=UTF-8")]
Body: <the response body>
```

## 深層情報

基本認証を含むHTTPリクエストを送信すると、認証情報が平文でエンコードされ、ヘッダー`Authorization: Basic <credentials>`に含まれます。"Basic"は、基本認証を使用していることを示します。

この方法は非常に歴史的なもので、送信される情報が暗号化されていないため安全ではないとされていますが、HTTPS経由で通信を行う場合（つまり、リクエストそのものが暗号化されている場合）は、依然として有効です。

代替手段としてトークンベースの認証やOAuthがありますが、実装の複雑さと進行中のメンテナンスが必要になるため、基本認証は簡単で速やかに認証を行う際の適切な選択肢です。

## 関連情報

1. [Haskellの基本認証](https://hackage.haskell.org/package/http-client-0.7.3/docs/Network-HTTP-Client.html)
3. [基本認証のセキュリティ上の警告](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Security)