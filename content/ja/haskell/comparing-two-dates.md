---
title:                "「二つの日付の比較」"
html_title:           "Haskell: 「二つの日付の比較」"
simple_title:         "「二つの日付の比較」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

日付同士を比較することに何の意味があるのでしょうか？このようなタスクは、コンピュータ上のデータを整理する際に便利です。例えば、あるイベントが特定の期間内に発生したかどうかを判断したり、期限を過ぎたタスクを見つける際に役立ちます。

## How To

まずは、`Data.Time`モジュールをインポートします。

```
```Haskell
import Data.Time
```

比較したい日付を作成します。日付の形式は、`UTCTime`型を使用します。

```
```Haskell
date1 = UTCTime (fromGregorian 2020 6 2) 0
date2 = UTCTime (fromGregorian 2020 6 3) 0
```

日付を比較する際には、`UTCTime`型の`compare`関数を使います。この関数は、第一引数が第二引数よりも前の日付であれば`LT`、同じ日付であれば`EQ`、後の日付であれば`GT`を返します。

```
```Haskell
compare date1 date2
-- Output: LT
```

また、日付を比較する際には、`utctDay`関数を使うこともできます。この関数は、与えられた`UTCTime`型のオブジェクトから日付を取り出し、`Day`型で返します。

```
```Haskell
utctDay date1
-- Output: 2020-06-02
```

## Deep Dive

`UTCTime`型は、基本的には「世界標準時（UTC）」での日時を表しますが、`localTimeToUTC`関数を介して、「現在地の標準時」の日時に変換することもできます。この関数には、現在地のタイムゾーンや夏時間などの情報を与える必要があります。

また、日付を比較する際に、単純に2つの日付を比較するだけではなく、特定の期間内にあるかどうかを判断したい場合もあります。そのような場合には、`addDays`関数を使うことで、指定した日数だけ日付を進めることができます。

```
```Haskell
weddingDate = UTCTime (fromGregorian 2021 6 5) 0
today = UTCTime (fromGregorian 2021 6 1) 0

-- 期間内にあるかどうかを判断
if utctDay today `addDays` 7 > utctDay weddingDate
    then putStrLn "結婚式まであと1週間です！"
    else putStrLn "結婚式までまだ時間がありますね。"
-- Output: 結婚式まであと1週間です！
```

## See Also

見本:
- [使用することができる日付に関する関数のリスト](https://www.haskell.org/hoogle/?hoogle=Date)
- [Haskellの日付および時間についてのチュートリアル](https://www.timeanddate.com/calendar/months/?year=2020)
- [Haskellの日付処理に関する公式ドキュメント](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)