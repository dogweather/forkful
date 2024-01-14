---
title:                "Elm: csv での作業"
simple_title:         "csv での作業"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを使うのか

CSVはデータを保存するための便利な形式です。データを整理しやすく、さまざまなプログラミング言語で読み取ることができます。

## 方法

まず、 `elm-csv`パッケージをインストールします。次に、CSVファイルを含むプロジェクトフォルダに移動し、`elm-csv`の`Csv.Decode`モジュールをインポートします。次に、CSVファイルをデコードして`Decoder`型を使用してデータを変換します。

例えば、以下のようなCSVファイルがあるとします：

```csv
Name, Age
John, 25
Mary, 30
```

次のようなコードを使用して、このCSVファイルをデコードすることができます：

```elm
import Csv.Decode exposing (Decoder, map2, field, string, int)

type alias User =
  { name : String
  , age : Int
  }

csvDecoder : Decoder User
csvDecoder =
  map2 User
    (field "Name" string)
    (field "Age" int)
    
users : Result String (List User)
users =
  Csv.Decode.decodeFile csvDecoder "users.csv"
```

このコードでは、`User`というエイリアスを作成し、`name`と`age`というフィールドを持つ、`User`型のデータを定義しています。`csvDecoder`は、`User`型のデータをデコードするための`Decoder`型の定義です。最後に、`decodeFile`関数を使用して、CSVファイルをデコードし、結果を`users`変数に格納します。

## 深い掘り下げ

`Csv.Decode`モジュールには、さまざまな関数や型が用意されており、より複雑なCSVファイルをデコードすることも可能です。また、エラーハンドリングやオプションのフィールドの処理など、さまざまな機能もありますので、詳しくは公式ドキュメントを参照してください。

## See Also

- [elm-csvパッケージの公式ドキュメント](https://package.elm-lang.org/packages/elm-explorations/csv/latest/)
- [Elm公式ガイド](https://guide.elm-lang.org/)