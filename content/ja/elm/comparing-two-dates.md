---
title:                "日付を比較する"
html_title:           "Elm: 日付を比較する"
simple_title:         "日付を比較する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

二つの日付を比較することが重要なのか、その理由はなんでしょうか？短く説明します。まず、日付の比較は日常生活でよく使われる機能であり、特にアプリケーション開発においても必要不可欠です。例えば、予定管理アプリではイベントの日付を比較することで、重複する予定を特定することができます。

## 方法

Elmを使用して、二つの日付を比較する方法を紹介します。まずは日付を文字列ではなく日付オブジェクトとして処理することが重要です。その後、日付オブジェクトのプロパティを使用して比較ロジックを実装します。

```elm
import Time
import Date

-- 日付オブジェクトの作成
date1 : Date
date1 = Date.fromText "2020-04-01"

date2 : Date
date2 = Date.fromText "2020-04-15"

date3 : Date
date3 = Date.fromText "2020-05-01"

-- 日付を比較する関数
compareDates : Date -> Date -> String
compareDates date1 date2 =
    if Date.compare date1 date2 == EQ then
        "同じ日付です"
    else if Date.compare date1 date2 == LT then
        Date.toString date1 ++ " より前の日付です"
    else
        Date.toString date1 ++ " より後の日付です"

-- 比較の実行と結果の表示
main : Html msg
main =
    div []
        [ text (compareDates date1 date2) -- "2020-04-01 より前の日付です"
        , text (compareDates date2 date3) -- "2020-04-15 より前の日付です"
        , text (compareDates date1 date1) -- "同じ日付です"
        ]
```

## 深堀り

日付の比較にはさまざまな方法がありますが、一般的には「年」→「月」→「日」の順番で比較することが推奨されています。Elmでは、内部的には日付をJulian Dayという数値で表現しているため、数値として比較することも可能です。また、日付オブジェクトにはサポートされている機能が多くあり、開発者は必要に応じてそれらを使用してより複雑な比較ロジックを実装することができます。

## おすすめのリンク

- [Dateモジュールのドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm公式ガイド](https://guide.elm-lang.jp/date-and-time/)
- [Julian Dayについての詳細な解説](https://www.maths.usyd.edu.au/u/olver/JDN.html)