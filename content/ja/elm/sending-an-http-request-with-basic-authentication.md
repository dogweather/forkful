---
title:                "Elm: 基本認証付きのhttpリクエストの送信"
simple_title:         "基本認証付きのhttpリクエストの送信"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
HTTPリクエストを基本認証で送信する理由は、セキュリティやプライバシー保護のためです。基本認証は、ユーザーの認証情報を暗号化して送信するため、無断アクセスやデータの漏洩を防げます。

## 方法
基本認証を使用してHTTPリクエストを送信するためには、次のステップを実行する必要があります。

1. `elm/http`パッケージをインポートする
2. `Http.send`関数を使用してリクエストを設定する
3. `Http.send`に認証情報が含まれるよう`Http.request`を使用する
4. リクエストを送信する

例えば、APIからデータを取得するGETリクエストを送信する場合、以下のようにコードを書くことができます。

```Elm
import Http
import Json.Decode exposing (..)

getUsersRequest : Http.Request
getUsersRequest =
  Http.get "https://api.example.com/users"
    |> Http.send Http.defaultSettings
    |> Http.request
  |> Http.sendAuthenticated BasicAuth "username" "password"

getUsers : Cmd Msg
getUsers =
  Http.send getUsersRequest
    |> Task.perform
      (\result ->
        case result of
          Ok response ->
            case Http.toString response of
              Json _ ->
                getUsersSucceed <| Http.fromJson decodeUsers response

          Err error ->
            getUsersFailed error
      )

type alias User =
  { id : Int
  , name : String
  , email : String
  }

type Msg
  = GetUsers
  | GetUsersSucceed (List User)
  | GetUsersFailed Http.Error

decodeUsers : Decoder (List User)
decodeUsers =
  decodeList (\obj ->
    User
      |> map obj
        (field "id" int
        , field "name" string
        , field "email" string
      )
  )

```

成功した場合は、`User`のリストが返されます。このように、基本認証を使用することで、ユーザーのデータを安全に取得することができます。

## 深堀り
基本認証を使用するには、`elm/http`パッケージで提供される`Http.sendAuthenticated`を使用します。しかし、この関数は`Http.request`と併用する必要があります。これは、`Http.send`に認証情報を含めるための特別なコンストラクターです。基本認証の認証情報は、ユーザー名とパスワードからなり、`BasicAuth`というタイプエイリアスを使用して設定します。これにより、安全に認証情報を設定することができます。

## 参考
- [elm/http パッケージドキュメント](https://package.elm-lang.org/packages/elm/http/latest/)
- [HTTPリクエストの認証](https://guide.elm-lang.jp/webapps/http_requests.html#authentication)