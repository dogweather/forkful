---
title:                "JSONを扱う方法"
date:                  2024-01-19
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
## なぜ & どうして？
JSONとはJavascript Object Notationの略で、データ交換のフォーマットです。プログラマは異なるシステム間でデータをやり取りするためにこれを使います。Elmでは、型の安全性を確保しながらJSONを扱うことができます。

## How to:
## 実践方法

```elm
-- JSONデコーダの定義
import Json.Decode exposing (Decoder, string, int, field)

type alias User =
    { name : String
    , age : Int
    }

userDecoder : Decoder User
userDecoder =
    Json.Decode.map2 User
        (field "name" string)
        (field "age" int)

-- JSONデータをElmの型に変換
jsonString : String
jsonString =
    "{\"name\":\"Yamada\",\"age\":30}"

decodedUser : Result String User
decodedUser =
    Json.Decode.decodeString userDecoder jsonString

-- 結果の出力
toString decodedUser
-- "Ok { name = \"Yamada\", age = 30 }"
```

## Deep Dive
## 掘り下げ

Elm での JSON 扱いは安全です。バージョン0.18からデコーダは強化され、より厳密な型チェックが可能になりました。`elm/json`ライブラリはJavaScriptのオブジェクトをElmの値に変換するために使います。Alternativesとして、HTTPライブラリを使ったAPIレスポンスの処理や、単純なデータ型ではないケースでのカスタムデコーダの実装があります。

## See Also
## 参考リンク

- Elm公式ガイド: [JSONデコード](https://guide.elm-lang.org/effects/json.html)
- `elm/json`パッケージ: [package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- JSONデコードの追加例: [ElmでのJSONデコードの技術](https://thoughtbot.com/blog/decoding-json-structures-with-elm)
