---
title:                "基本認証を使用したhttpリクエストの送信"
html_title:           "Elm: 基本認証を使用したhttpリクエストの送信"
simple_title:         "基本認証を使用したhttpリクエストの送信"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何ができるの？
HTTPリクエストを基本認証付きで送信することは、サーバーからデータを取得するための一般的な方法です。プログラマーがこの方法を選ぶのは、情報を保護するためのセキュリティ上の理由や、アクセス制限されたコンテンツにアクセスする必要があるからです。

## 方法：
```Elm
import Http
import Basics exposing (..)
import Json.Decode as Json

authReq : Http.Request
authReq =
    Http.request
        { url = "https://example.com/api/endpoint"
        , method = "GET"
        , headers = [ ( "Authorization", "Basic QWxhZGRpbjpPcGVuU2VzYW1l" ) ]
        , body = Http.emptyBody
        , expect = Http.expectString Json.decoder
        , timeout = Nothing
        , tracker = Nothing
        }
```

```Elm
import Http
import Basics exposing (..)
import Json.Decode as Json

authResponse : Http.Response String
authResponse =
    Http.fromString "Unauthorized: Invalid credentials"
```

## 深堀り：
1. プログラマーがHTTPリクエストを基本認証付きで送信する方法は、以前はよりセキュアな方法がなかったためでした。しかし、現在はより安全かつ効率的な方法が存在します。
2. 基本認証に代わる代替手段としては、OAuthがあります。OAuthは、サードパーティーAPIへのアクセスを許可するためのセキュリティプロトコルです。
3. HTTPリクエストを送信する際の基本認証の詳細な実装方法は、プログラマーが使用するフレームワークや言語によって異なりますが、一般的にはヘッダーにユーザー名とパスワードをエンコードして送信することになります。

## 関連情報：
- [Elm公式ドキュメンテーション](https://package.elm-lang.org/packages/elm/http/latest/Http)
- [HTTP基本認証についての詳細記事](https://www.digitalocean.com/community/tutorials/http-basic-authentication-with-nginx-on-ubuntu-14-04)
- [OAuthプロトコルの詳細](https://oauth.net/2/)