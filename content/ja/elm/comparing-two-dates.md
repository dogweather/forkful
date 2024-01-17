---
title:                "2つの日付の比較"
html_title:           "Elm: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

日付を比較することは、プログラマーにとって重要なタスクの一つです。例えば、古いデータを最新のものと比較し、更新の必要性を判断することがあります。また、特定の日付が他よりも前か後ろかを確認することもあります。プログラマーは日付を比較することで、データの整合性を保つことができます。

## 方法:

```Elm 
import Date exposing (fromString, before)
 
date1 = String.fromList [ ”2020”, ”07”, ”01” ]
date2 = String.fromList [ ”2020”, ”07”, ”15” ]
 
main =
    case (fromString date1, fromString date2) of
        (Just d1, Just d2) ->
            if before d1 d2 then
                "date1 comes before date2"
            else
                "date2 comes before date1"
        _ ->
            "invalid date"
```

```
date1 comes before date2
```

## 深堀り:

- 日付を比較するための関数は、どの言語でも利用可能ですが、Elmの場合は型安全性が高く、エラーが起きにくいという利点があります。
- 別の方法として、日付を数字の配列として比較する方法があります。しかし、この方法ではより多くのエラーが発生しやすく、データの整合性が保たれにくいという欠点があります。
- Elmでは、日付を扱うためのDate型が用意されており、それを利用することでより安全に、正確に日付を比較することができます。

## 関連情報:

- [Elm公式ドキュメント: Date](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [日付を比較する方法: Elm vs JavaScript](https://medium.com/@pablovilloslada/comparing-dates-with-elm-vs-javascript-4a3e7318bab1)