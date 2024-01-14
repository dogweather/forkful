---
title:    "Gleam: 「将来または過去の日付の計算」"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算することに関わる理由は、将来の予定を立てる際や過去の出来事を振り返る際に役立ちます。また、日付を計算することによって、カレンダーやイベント管理を行う上での便利な手段となります。

## 方法

```Gleam
let date_in_future = Calendar.AddDays(2021-09-01, 7)
```

このコードは、今日から1週間後の日付を計算する例です。上記のように、`Calendar.AddDays`関数を使用することで、未来や過去の日付を計算することができます。また、計算する単位（日、週、月、年）を指定することも可能です。

```Gleam
let date_in_past = Calendar.AddMonths(2021-09-01, -3)
```
上記のコードは、今日から3ヶ月前の日付を計算する例です。同様に、`AddMonths`関数を使用することで、月単位での日付の計算が可能です。

## より深い理解

日付計算には、さまざまな方法があります。例えば、特定の日付からの経過日数を計算する方法や、2つの日付の差を計算する方法があります。日付計算を行う際は、使用する関数や引数を適切に選択することが重要です。

## その他

[日付を計算するためのGleamドキュメント](https://gleam.run/documentation/stdlib/calendar.html#add-days)

[日付計算に関するGleamフォーラム](https://gleamverse.com/t/how-to-calculate-date-difference/44)

## また見る

[Gleamの日付操作について学ぶ](https://it.srad.jp/story/20/08/10/0258246/1031813/)