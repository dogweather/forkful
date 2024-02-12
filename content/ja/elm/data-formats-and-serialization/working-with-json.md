---
title:                "JSONを活用する"
aliases:
- /ja/elm/working-with-json/
date:                  2024-02-03T19:22:56.161320-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
ElmでJSONを扱うことは、JSONデータをElmの型にデコードし、Elmの値をJSONにエンコードするプロセスです。このプロセスは、ウェブアプリケーションがAPIや外部データソースとやり取りするために不可欠であり、クライアント（Elm）とサーバーやその他のサービス間でのデータのシームレスなやり取りを可能にします。

## 方法:

ElmはJSONの取り扱いを明示性と安全性をもって扱います。主に`Json.Decode`モジュールと`Json.Encode`モジュールを使用します。JSONを扱い始めるには、まずデータタイプのためのデコーダを定義する必要があります。ここでは、シンプルなユーザープロファイルオブジェクトを扱っていると仮定しましょう。

まず、Elmの型を定義します：

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### JSONをElmにデコードする

JSON文字列を`UserProfile`型にデコードするためにデコーダを作成します：

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

JSONオブジェクトをデコードするには：

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- サンプル出力:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### ElmをJSONにエンコードする

Elmの値をJSONに戻すために、`Json.Encode`モジュールを活用します。

```elm
import Json.Encode exposing (object, int, string)

encodeUserProfile : UserProfile -> String
encodeUserProfile userProfile =
    object
        [ ("id", int userProfile.id)
        , ("name", string userProfile.name)
        , ("email", string userProfile.email)
        ]
        |> Json.Encode.encode 0

{-
使用例:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

サンプル出力:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### サードパーティライブラリ

`elm-json-decode-pipeline`のようなElmパッケージは、パイプラインスタイルを用いてデコーダの作成を簡単にすることができ、複雑なオブジェクトのデコードに特に便利です。

まず、ライブラリをプロジェクトに追加します：

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

次に、デコーダの定義を次のように簡素化できます：

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- 以前と同様、decodeStringを使ってJSON文字列をデコードするためにこのデコーダを使用します。-}
```

このアプローチはデコーダを単純化し、コードをよりクリーンで保守しやすくし、特にデータ構造が複雑になるにつれてその価値が増します。
