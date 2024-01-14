---
title:                "Haskell: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日時を比較することは、日常生活の中でよく行われる作業です。プログラムを書いていると、時には異なる日時の比較が必要になることがあります。そのような場合、Haskellの関数を使って簡単に日時を比較することができます。

## 方法

日時を比較するには、Haskellの標準ライブラリである"Data.Time"モジュールを使用します。最初に、比較したい2つの日時をそれぞれをオブジェクトとして定義します。例えば、以下のようになります。

```Haskell
let date1 = UTCTime (fromGregorian 2021 1 1) (secondsToDiffTime 3600) -- 2021年1月1日 01:00:00
let date2 = UTCTime (fromGregorian 2022 1 1) (secondsToDiffTime 0) -- 2022年1月1日 00:00:00
```

次に、"diffUTCTime"関数を使って日時の差分を取得します。この関数は、差分を秒単位で表すことができます。例えば、今回の場合は、以下のようになります。

```Haskell
diffUTCTime date2 date1 -- 31536000秒 (1年)
```

より複雑な比較をする場合は、"Day"データ型を使って日付のみを比較することもできます。以下の例では、"toGregorian"関数を使って"Day"を"(year, month, day)"のタプルに変換してから比較しています。

```Haskell
let date3 = modifiedJulianDay 58849 -- 2021年1月1日
let date4 = modifiedJulianDay 61553 -- 2022年1月1日
if toGregorian date3 < toGregorian date4 then "date4はdate3よりも後です" else "date3はdate4よりも後です" -- "date4はdate3よりも後です"
```

## 詳細を深掘り

日時を比較する際に気をつける点は、時差の影響を受けることです。例えば、日本で2022年1月1日に実行した場合と、グリニッジ標準時で同じ日時に実行した場合とでは、差分が異なってしまいます。そのような場合は、"zonedTimeToUTC"関数を使って時差を考慮した日時を取得する必要があります。

また、"diffUTCTime"関数は、2つの日時の差分を秒単位で表しますが、より詳細な比較が必要な場合は、"diffDays"や"diffTimeOfDay"関数を使うことで、日数や時間単位での差を取得することもできます。

## 関連リンク

- [Haskell Data.Timeモジュールのドキュメント](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/Data-Time.html)
- [HourlyUTCTimeライブラリ](http://hackage.haskell.org/package/HourlyUTCTime)
- [zonedTimeToUTC関数の例](https://stackoverflow.com/questions/5820617/convert-local-date-into-utc-in-haskell-data-time-format)