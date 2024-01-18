---
title:                "文字列から日付を解析する"
html_title:           "Elm: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何をするか & なぜするか？
日付を文字列からパースすることは、プログラマーにとって非常に重要な作業です。パースとは、文字列から特定の形式のデータを抽出することを指します。日付を文字列からパースするためには、文字列内の日付の形式を認識し、それを日付オブジェクトとして扱う必要があります。プログラマーがこれを行う理由は、データの取得や処理を簡単にするためです。

## やり方：
Elmを使って日付を文字列からパースするサンプルコードを以下に示します。

```elm
import Time exposing (Posix)
import Date.Extra exposing (Date, fromIsoString)

-- 文字列から日付をパースする
getParsedDate : String -> Date
getParsedDate str =
  fromIsoString str

-- 日付を文字列からパースする関数の実行例
getParsedDate "2021-04-23" -- 結果: Apr 23, 2021

-- 文字列から日付と時刻をパースする
getParsedDateTime : String -> Date
getParsedDateTime str =
  case str of
    "2021-04-23 12:00:00" ->
      Posix.fromEpoch 0
    "2021-04-23 18:00:00" ->
      Posix.fromEpoch 6
    _ ->
      Posix.fromEpoch 12
```

## より詳しく：
日付を文字列からパースする方法は、プログラミング言語によって異なりますが、基本的な考え方は同じです。日付を文字列からパースするには、まず文字列内の日付の形式を認識する必要があります。その後、認識した日付の形式を基に、日付オブジェクトとして扱うコードを記述します。日付を文字列からパースする方法としては、上記のように標準ライブラリの関数を使う方法や、外部ライブラリを使う方法があります。

## 関連情報：
- [Elm Timeパッケージ](https://package.elm-lang.org/packages/elm/time/latest/)：Timeパッケージには日付を扱うための便利な関数が含まれています。
- [Elm Date.Extraパッケージ](https://package.elm-lang.org/packages/elm-community/date-extra/latest/)：Date.Extraパッケージには、さまざまな日付を扱うための関数が含まれています。
- [Elm日付の操作方法のドキュメント](https://elm-lang.org/docs/), https://elm-lang.org/docs/packages/elm/time/latest/Time
：Elmの公式ドキュメントには日付を操作するための詳細な情報が記載されています。