---
title:                "「csvを使うことについて」"
html_title:           "Elm: 「csvを使うことについて」"
simple_title:         "「csvを使うことについて」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを扱うのか

CSVは多くのプログラムでデータを取り扱うための標準形式です。データを取り込む時やファイルをエクスポートする時に便利なフォーマットなので、エルムを使う上で重要なスキルです。

## やり方

すぐにコードを書き始めるために、まずはCSVパッケージをインポートします。

```Elm
import Csv
```

次に、CSVファイルを読み込んでリストとして取得する方法を見ていきましょう。

```Elm
readCsv : Csv.File -> Result Csv.Error (List (List Csv.Field))
readCsv file =
  Csv.parse file
    |> Result.mapError (always CannotParse)
```

このように、`Csv.parse`関数を使うことで、CSVファイルをパースしてリストとして取得することができます。詳しい使い方は[公式ドキュメント](https://package.elm-lang.org/packages/elm-community/csv/latest/)を参照してください。

CSVを取得した後は、通常のリスト操作が可能です。例えば、次のようにしてデータをフィルタリングすることができます。

```Elm
filterRowsByColumn : String -> List (List Csv.Field) -> List (List Csv.Field)
filterRowsByColumn value rows =
  List.filter (\row -> List.member value row) rows
```

さらに詳しい例やCSVファイルを書き込む方法などは、[こちらのブログ記事](https://thoughtbot.com/blog/using-elm-with-csv-files)を参考にしてください。

## 詳しく見る

この記事では、基本的なCSVの扱い方を紹介しましたが、実際にはもっと複雑なデータ構造を扱う場合があります。そんな時には[elm-csv-decode](https://github.com/ryannhg/elm-csv-decode)パッケージが役に立つかもしれません。また、エラー処理やバリデーションを行いたい場合には、[elm-validate](http://package.elm-lang.org/packages/rtfeldman/elm-validate/1.2.1/)パッケージを使うこともできます。

## 参考リンク

- [Elm 0.19 公式ドキュメント](https://guide.elm-lang.jp/)
- [公式パッケージリスト](https://package.elm-lang.org/)