---
title:                "HTTPリクエストの送信"
html_title:           "Elm: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信することになるかをお話しします。これは、Webアプリケーションでサーバーへデータをリクエストするために必要なスキルです。例えば、フォームから入力したデータをサーバーに送信したり、APIを使用して外部のデータを取得する際に使用します。

## 作り方

ElmでHTTPリクエストを送信するには、**Http**モジュールを使用します。以下のようにインポートします。

```Elm
import Http
```

次に、リクエストを送信するためのデータ構造を定義します。

```Elm
request : Http.Request
request = 
  { method = "GET"
  , url = "https://example.com/api/data"
  , body = Http.emptyBody
  , headers = []
  }
```

**request**の中には、リクエスト方法や送信先のURL、ヘッダーなどを指定できます。また、HTTPリクエストでデータを送信する際は、ボディの値を指定する必要があります。例えば、以下のようにJSONデータを送信することができます。

```Elm
import Json.Encode as Json

requestWithBody : Http.Request
requestWithBody = 
  { method = "POST"
  , url = "https://example.com/api/data"
  , body = Http.jsonBody 
      (Json.object 
        [ 
          ( "name", Json.string "John" )
        , ( "age", Json.int 25 )
        ]
      )
  , headers = [( "Content-Type", "application/json" )]
  }
```

これでリクエストの準備ができました。最後に、**Http.send**関数を使用してリクエストを送信します。

```Elm
import Html exposing (text)

sendRequest : Cmd Msg
sendRequest =
  Http.send handleResponse requestWithBody

handleResponse : Http.Response Json.Decoder
handleResponse =
  case response of
    Http.Timeout ->
      text "Request timed out"

    Http.NetworkError ->
      text "Network error"

    Http.BadStatus statusCode ->
      text ("Received bad status code: " ++ String.fromInt statusCode)

    Http.GoodStatus response ->
      Json.decodeString response.body
          |> Result.map GotData

type Msg
  = GotData (Result Http.Error (List Book))

type alias Book =
  { title : String
  , author : String
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotData result ->
      case result of
        Ok books ->
          -- Do something with the list of books
        Err err ->
          -- Handle error case
```

以上でHTTPリクエストの作り方がわかりました。詳しくは、公式ドキュメントをお確かめください。

## ディープダイブ

さらに詳しくHTTPリクエストについて学ぶには、サーバーからのリクエストの扱い方や、エラーハンドリングなどを学ぶ必要があります。また、Elmでは**Task**モジュールを使用することで、より詳細なリクエスト処理を行うことができます。詳しくは、公式ドキュメントを確認してみてください。

## 参考リンク

- Elm公式ドキュメント: https://guide.elm-lang.org/
- JSONパッケージ: https://package.elm-lang.org/packages/elm/json/latest/
- Taskモジュール: https://package.elm-lang.org/packages/elm/core/latest/Task