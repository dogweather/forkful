---
title:                "Elm: 「HTTPリクエストを送る」"
simple_title:         "「HTTPリクエストを送る」"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信することの意義を説明するためには、Web開発においてサーバーとの通信が重要であることを説明します。データの送受信を容易にするためには、HTTPリクエストを送信する必要があります。

## 方法

```Elm
import Http
import Json.Decode exposing (..)

type alias User = {
  name : String,
  age : Int
}

getUser : Http.Request User
getUser =
  Http.get "https://api.example.com/users/1"
    , userDecoder

userDecoder : Decoder User
userDecoder =
  Decode.map2 User
    (field "name" string)
    (field "age" int)
```

上記のコードでは、`Http` モジュールをインポートし、`Json.Decode` モジュールから必要な関数をエクスポートしています。そして、`User` というレコード型を作成し、サーバーから受け取ったデータをデコードするための関数 `getUser` を定義しています。`Http.get` 関数を使用して、特定のURLに対して GET リクエストを送信し、`userDecoder` でデータをデコードしています。

データを取得した後は、以下のようにデータを表示することができます。

```Elm
Http.send getUser ReceivedUser

type Msg
  = ReceivedUser (Result String User)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReceivedUser result ->
      case result of
        Ok user ->
          -- userのデータを表示する
        Err err ->
          -- エラーを処理する
```

ここでは、`ReceivedUser` というメッセージを受け取ると、`Ok` と `Err` でパターンマッチングを行い、`user` データを表示するかエラーを処理するように定義しています。

## ディープダイブ

HTTPリクエストは、Webアプリケーションで必要不可欠な機能です。例えば、ユーザーがフォームを送信した際に、そのデータをサーバーに送信することができます。また、サーバーからデータを受け取ることで、リアルタイムなアップデートが可能になります。

また、Elmでは`Http.Request` だけでなく、`Http.expectString` や `Http.expectJson` など、さまざまな関数を使用してHTML、イメージ、ファイルなどをリクエストすることもできます。また、HTTPリクエストをエンコードする際には、エンコード方法やヘッダーを指定することも可能です。

## あわせて見る

### ドキュメント

- [Elm HTTPモジュール](https://package.elm-lang.org/packages/elm/http/latest)
- [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)

### カンファレンス

- [Elm Conf](https://www.elm-conf.com/)
- [Elm in the Spring](https://www.elminthespring.org/)

### オンラインコース

- [Learn Elm in Y minutes](https://learn-elm.yanis.dev/)
- [Frontend Masters - Beginning Elm](https://frontendmasters.com/courses/beginning-elm/)