---
title:                "Elm: JSONの取り扱いについて"
simple_title:         "JSONの取り扱いについて"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ

JSONは、データを簡単に受け渡しするための最も一般的な形式であり、Elmにおいても非常に重要な役割を担っています。Elmを使ってJSONを処理することにより、データのフロントエンドとバックエンドの間でのスムーズなデータのやりとりが可能になります。

## 使い方

以下のコードは、JSONに関する基本的な処理方法を示しています。

```Elm
import Http
import Json.Decode exposing (..)

-- JSONを取得するリクエストを作成
request : Http.Request String
request =
    Http.get "https://example.com/data.json"

-- JSONをデコードして、データを取得
-- ここでは、データの型をStringとして指定していますが、適宜変更してください
-- Json.Decode.string以外にも、numberやbool, listなど様々な関数があります
getData : Result Http.Error String
getData =
    Http.send request
        |> Task.attempt (Json.Decode.decodeString Json.Decode.string)

-- デコードされたデータを表示
case getData of
    Ok data ->
        data |> Debug.log "Decoded data: "

    Err _ ->
        Debug.log "Error decoding data"
```

上記のコードでは、まずHttpモジュールをインポートし、URLからJSONを取得するリクエストを作成します。次に、Json.Decodeモジュールを使用して、デコードしたいデータの型を指定し、データを取得します。最後に、取得したデータを表示するようにしています。

## 深堀り

JSONを扱う際に覚えておきたい重要なことの一つは、データの型を明示的に指定することです。上記のコードでは、データの型としてStringを指定しましたが、必要に応じて変更してください。また、Json.Decodeモジュールには、データをより細かく扱うための関数が多数用意されていますので、是非調べてみてください。

## 関連リンク

- [ElmでJSONを扱う](https://guide.elm-lang.jp/interop/json.html)
- [Httpモジュール](https://elm-lang.org/docs/advanced/http)
- [Json.Decodeモジュール](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)