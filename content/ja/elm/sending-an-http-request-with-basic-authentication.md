---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "Elm: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

基本認証を使ったHTTPリクエストを送信する理由は、ウェブアプリケーションやモバイルアプリケーションなど、エンドユーザーが保護されたページやデータにアクセスする必要がある場合に利用されます。

## 使い方

基本認証を使用してHTTPリクエストを送信するには、まず [elm-lang/http](https://package.elm-lang.org/packages/elm-lang/http/latest/) パッケージをインストールする必要があります。次に、`send` 関数を使用してリクエストを送信します。例えば、以下のコードは認証情報を含むヘッダーを持つGETリクエストを送信する方法を示しています。

```elm
import Http
import Json.Decode as Decode

request : Http.Request Decode.Value
request =
  Http.get
    { url = "https://example.com/api/data"
    , headers = [ Http.basicAuth "username" "password" ]
    , expect = Http.expectJson Decode.value
    }
```

このコードの実行結果として返されるレスポンスは、JSONデータをデコードすることができる `Decode.Value` 型となります。

## ディープダイブ

基本認証は、サーバーがログイン資格情報を要求し、ユーザーが提供することで認証を行う方法です。一般的に、認証情報はヘッダーに `Authorization` フィールドとして含まれ、データは `username:password` の形式でエンコードされます。しかし、セキュリティ上の理由から、認証情報をプレーンテキストで送信するのは推奨されません。代わりにベーシック認証を使用する場合は、HTTPSを使って送信することが重要です。

## 関連リンク

- [elm-lang/http パッケージのドキュメント](https://package.elm-lang.org/packages/elm-lang/http/latest/)
- [HTTPリクエストの送信に関する記事 (英語)](https://guide.elm-lang.org/effects/http.html)
- [elm/http-extras パッケージ](https://package.elm-lang.org/packages/elm/http-extras/latest/)（カスタムヘッダーやクッキーのような追加機能を備えたHTTPリクエストの送信に使用されるパッケージ）