---
title:    "Elm: 2つの日付の比較"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
なぜ日付を比較するのか？

日付を比較することは、プログラミングでよくあるタスクです。例えば、入力された日付が未来のものかどうかをチェックする場合などに必要になります。Elmは型安全な言語であり、日付を比較する際にも型が一致しているかどうかをチェックすることができます。

## How To

日付を比較する方法は、単純です。まず、比較したい2つの日付をDate型として宣言します。次に、Dateモジュールの`compare`関数を使用し、比較結果を得ることができます。例えば、以下のコードでは、今日の日付と明日の日付を比較しています。

``` elm
import Date exposing (..)

today : Date
today = Date.fromCalendarDate 2020 9 21

tomorrow : Date
tomorrow = Date.fromCalendarDate 2020 9 22

compareResult : DateOrder
compareResult = compare today tomorrow -- LTという結果が得られる
```

ここで使用した`DateOrder`型は、比較結果を表す型であり、3つの可能な値があります。`LT`は前の日付が後の日付よりも早いことを表し、`GT`は後の日付が前の日付よりも早いことを表します。そして、`EQ`は2つの日付が等しいことを表します。

## Deep Dive

日付を比較する際には、注意すべきポイントがいくつかあります。

まず、`compare`関数は2つの日付を比較するだけではなく、3つ以上の日付を比較することもできます。その際は、最初の2つの日付を比較した結果に基づいて、残りの日付を比較します。

また、上記のコードでは、明示的に`LT`という比較結果を得るために`compare`関数を使用しましたが、Elmでは演算子を使用することでも比較が可能です。例えば、`>`, `<`, `==`のような演算子を使用することで、それぞれ`GT`, `LT`, `EQ`と同じ結果を得ることができます。

さらに、`Date`モジュールには`equals`や`compareValues`といった比較用の関数が用意されています。これらの関数は、日付の情報が同じかどうかをチェックすることができます。また、異なる日付表現を比較する際には、`Date.Extra`モジュールの関数を使用することで、日付を同じ表現に変換して比較することができます。

## See Also

- [Official Elm Date documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Specific date comparison in Elm](https://reactgo.com/elm-date-comparison/)