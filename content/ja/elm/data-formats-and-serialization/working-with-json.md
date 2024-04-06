---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:56.161320-07:00
description: "\u65B9\u6CD5: JSON\u6587\u5B57\u5217\u3092`UserProfile`\u578B\u306B\u30C7\
  \u30B3\u30FC\u30C9\u3059\u308B\u305F\u3081\u306B\u30C7\u30B3\u30FC\u30C0\u3092\u4F5C\
  \u6210\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.924102-06:00'
model: gpt-4-0125-preview
summary: "Elm\u306FJSON\u306E\u53D6\u308A\u6271\u3044\u3092\u660E\u793A\u6027\u3068\
  \u5B89\u5168\u6027\u3092\u3082\u3063\u3066\u6271\u3044\u307E\u3059\u3002\u4E3B\u306B\
  `Json.Decode`\u30E2\u30B8\u30E5\u30FC\u30EB\u3068`Json.Encode`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002JSON\u3092\u6271\u3044\u59CB\
  \u3081\u308B\u306B\u306F\u3001\u307E\u305A\u30C7\u30FC\u30BF\u30BF\u30A4\u30D7\u306E\
  \u305F\u3081\u306E\u30C7\u30B3\u30FC\u30C0\u3092\u5B9A\u7FA9\u3059\u308B\u5FC5\u8981\
  \u304C\u3042\u308A\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u30B7\u30F3\u30D7\
  \u30EB\u306A\u30E6\u30FC\u30B6\u30FC\u30D7\u30ED\u30D5\u30A1\u30A4\u30EB\u30AA\u30D6\
  \u30B8\u30A7\u30AF\u30C8\u3092\u6271\u3063\u3066\u3044\u308B\u3068\u4EEE\u5B9A\u3057\
  \u307E\u3057\u3087\u3046."
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
