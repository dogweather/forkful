---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストに基本認証を送信するとは、Webサーバーに対してユーザー名とパスワードを含む特定の形式の情報をリクエスト中に送信することです。プログラマーがこれを行う主な理由は、特定のリソースにアクセスするための認証を提供することです。

## 使い方：

```Elm
import Http
import Http.BasicAuth as BasicAuth


sendRequest : String -> String -> String -> Cmd msg
sendRequest username password url =
    Http.request
        { method = "GET"
        , headers = [ BasicAuth.basic username password ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever
        , timeout = Nothing
        , withCredentials = False
        }

-- 使用例

main =
    sendRequest "user" "password" "https://myserver.com"

```
このコードは`user`と`password`という基本認証の詳細を`https://myserver.com`にGETリクエストとして送信します。

## 詳細：

基本認証はHTTPプロトコルの一部として1996年に導入され、以来Webサーバーとクライアント間でのシンプルな認証方法として広く使用されています。しかし、状況によってはBearer認証やDigest認証のような他の方法が適していることもあります。一方、Elmでは`Http.request`メソッドを用いて基本認証付きのHTTPリクエストを送信します。該当のメソッドを用いることでユーザー名とパスワードの詳細を含んだヘッダーを含めることが可能となります。

## 参考になるもの：

- ElmのHTTPリクエスト: https://package.elm-lang.org/packages/elm/http/latest/
- BasicAuthモジュール: https://package.elm-lang.org/packages/elm/http/latest/Http-BasicAuth
- ElmでのHTTP認証: https://guide.elm-lang.org/effects/http.html