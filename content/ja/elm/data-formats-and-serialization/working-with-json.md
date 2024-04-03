---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:56.161320-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.036373-06:00'
model: gpt-4-0125-preview
summary: "Elm\u3067JSON\u3092\u6271\u3046\u3053\u3068\u306F\u3001JSON\u30C7\u30FC\u30BF\
  \u3092Elm\u306E\u578B\u306B\u30C7\u30B3\u30FC\u30C9\u3057\u3001Elm\u306E\u5024\u3092\
  JSON\u306B\u30A8\u30F3\u30B3\u30FC\u30C9\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3059\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\u3001\u30A6\u30A7\u30D6\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304CAPI\u3084\u5916\u90E8\u30C7\u30FC\
  \u30BF\u30BD\u30FC\u30B9\u3068\u3084\u308A\u53D6\u308A\u3059\u308B\u305F\u3081\u306B\
  \u4E0D\u53EF\u6B20\u3067\u3042\u308A\u3001\u30AF\u30E9\u30A4\u30A2\u30F3\u30C8\uFF08\
  Elm\uFF09\u3068\u30B5\u30FC\u30D0\u30FC\u3084\u305D\u306E\u4ED6\u306E\u30B5\u30FC\
  \u30D3\u30B9\u9593\u3067\u306E\u30C7\u30FC\u30BF\u306E\u30B7\u30FC\u30E0\u30EC\u30B9\
  \u306A\u3084\u308A\u53D6\u308A\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
