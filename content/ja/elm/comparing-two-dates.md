---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

### 何となぜ？

日付の比較とは、時間の経過を計測するためにプログラマーが二つの日付を比較することです。これにより、予定の管理や達成度の追跡など、タイムリーな情報の処理が可能になります。

### どうするか：

それでは、Elmで日付を比較する方法を見てみましょう。

```Elm
import Time exposing (toMillis)
import Date exposing (..)

date1 = Date.fromTime (toMillis { year = 2021, month = Apr, day = 1, hour = 12, minute = 0, second = 0, millisecond = 0 })
date2 = Date.fromTime (toMillis { year = 2021, month = Apr, day = 2, hour = 12, minute = 0, second = 0, millisecond = 0 })

compareDates = Date.compare date1 date2

main=
    case compareDates of
        LT -> "date1 is earlier"
        GT -> "date1 is later"
        EQ -> "both dates are the same"
```
このコードを実行すると、結果は "date1 is earlier" と表示されます。それは、date1がdate2よりも早いからです。

### 深掘り：

日付の比較はプログラミングの歴史の初期から存在しており、あらゆるデータドリブンのアプリケーションで一般的です。他の言語やフレームワークでは、このタスクを行うためのさまざまな手法が提供されていますが、Elmでは`Date.compare`関数を使用します。

この関数は2つの日付を引数として取り、それらが等しい場合はEQ、最初の日付が2つ目の日付より先であればLT、最初の日付が2つ目の日付より遅れていればGTを返します。

### 参考情報：

次に、日付の比較に関する他の詳細情報へのリンクをいくつか紹介します：

1. [Elm公式ドキュメンテーション：Date](https://package.elm-lang.org/packages/elm/time/latest/Date)
2. [StackOverflow：Elmでの日付比較](https://stackoverflow.com/questions/52920077/comparing-dates-in-elm)