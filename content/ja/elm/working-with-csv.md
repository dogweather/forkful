---
title:                "「csvとの作業」"
html_title:           "Elm: 「csvとの作業」"
simple_title:         "「csvとの作業」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-csv.md"
---

{{< edit_this_page >}}

ElmでCSVを扱う: なぜして、どうする？

## What & Why?

CSVとは、データを表形式で表現するためのフォーマットです。プログラマーはこれを使用する理由として、データの整形や処理を容易にするために利用します。

## How to:

Elmでは、CSVを簡単に扱うことができます。以下のコードを使用して、CSVファイルを読み込み、データをテーブル形式で表示することができます。

```Elm
import Csv
import Html

type alias Row = List String

type alias Table = List Row

data : Csv.Decode.Result (Table) 
data =
    Csv.Decode.decodeString "id,name,age\n1,John,30\n2,Amy,25"

viewRow : Row -> Html.Html msg
viewRow row =
    Html.tr []
        (List.map (\ col -> Html.td [] [Html.text col]) row)

viewTable : Table -> Html.Html msg
viewTable table =
    Html.table []
        (Html.tr []
            [ Html.th [] [Html.text "ID"], Html.th [] [Html.text "Name"], Html.th [] [Html.text "Age"] ]
            :: List.map viewRow table
        )

main : Html.Html Msg
main =
    case data of
        Ok table ->
            viewTable table
        Err err ->
            Html.text "Error: " ++ (Csv.Decode.errorToString err)

```

このコードを実行すると、以下のようなテーブルが表示されます。

| ID | Name | Age |
|----|------|-----|
| 1  | John | 30  |
| 2  | Amy  | 25  |

## Deep Dive

CSVは、Comma-Separated Valuesの略称で、テキストファイルとして保存されます。パラメーターをコンマで区切り、改行文字で行を区切ってデータを表現します。

CSVは、テキストエディターで作成することもできますが、プログラムで処理することもできます。代替手段として、JSONやXMLなどのデータフォーマットもありますが、CSVはシンプルで取り扱いやすいため、よく利用されます。

Elmでは、[Csvパッケージ](https://package.elm-lang.org/packages/elm-community/csv/latest/Csv)を使用してCSVデータを扱うことができます。また、[elm-parser](https://package.elm-lang.org/packages/elm/parser/latest/)を使うと、独自のCSVパーサーを作成することもできます。

## See Also

- [Csvパッケージ](https://package.elm-lang.org/packages/elm-community/csv/latest/Csv)
- [elm-parser](https://package.elm-lang.org/packages/elm/parser/latest/)
- [CSVファイルの形式](https://ja.wikipedia.org/wiki/CSV_(%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E5%BD%A2%E5%BC%8F))