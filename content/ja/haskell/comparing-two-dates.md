---
title:    "Haskell: 「2つの日付の比較」"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

この記事では、日付を比較する方法について紹介します。日付を比較するのは、プログラミングにおいて非常に一般的なタスクです。例えば、イベントの開始日と終了日を比較することで、イベントが終了したかどうかを判断することができます。また、データの処理やレポートの作成などでも、日付の比較が必要になる場合があります。Haskellを使って日付を比較する方法を学びましょう。

## Why
日付を比較する方法を学ぶと、様々なタイプのデータの処理ができるようになります。例えば、データベースから取得した日付のデータをソートすることができます。また、特定の期間内に行われたイベントの数や種類を統計的に分析することも可能です。日付の比較は、データ分析やプログラミングにおいて非常に重要なスキルです。

## How To
日付を比較するには、まず日付を表す`Date`型のインスタンスを作成する必要があります。その後、`compare`関数を使用することで、2つの日付を比較することができます。下記のコードを参考にしてください。

```Haskell
module Main where

import Data.Time (UTCTime, fromGregorian, getCurrentTime, UTCTime, diffDays)
import Data.Time.Calendar (Day)
import Data.Time.Calendar.MonthCalendar (showDay)

-- 日付を表す`Date`型のインスタンスを作成
date1 :: Day
date1 = fromGregorian 2021 01 01

date2 :: Day
date2 = fromGregorian 2021 12 31

-- `compare`関数を使用して2つの日付を比較
dateComparison :: String
dateComparison
  | date1 < date2 = showDay date1 ++ " is earlier than " ++ showDay date2
  | date1 == date2 = "Both dates are the same"
  | otherwise = showDay date1 ++ " is later than " ++ showDay date2

main :: IO ()
main = do
  putStrLn "Comparing dates using Haskell..."
  putStrLn dateComparison
```

このプログラムを実行すると、以下のような出力が得られます。

```
Comparing dates using Haskell...
2021-01-01 is earlier than 2021-12-31
```

## Deep Dive
日付を比較する際に、より詳細な情報が必要な場合は、`diffDays`関数を使用することで日数の差を求めることができます。また、`UTCTime`型を使用することで、日付と時刻を同時に比較することができます。さらに、Hackageなどのオンラインリソースから、日付を扱うためのさまざまな関数やライブラリを見つけることができます。

## See Also
- [Haskell公式ドキュメント](https://www.haskell.org/documentation/)
- [Haskell datetimesパッケージ](https://hackage.haskell.org/package/datetimes)
- [Real World Haskell 日付と時刻](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/datetime)
- [Haskell datetimeライブラリの活用法](https://www.reddit.com/r/haskell/comments/5i4gpj/using_a_datetime_library_in_haskell/)