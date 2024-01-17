---
title:                "2つの日付の比較"
html_title:           "Haskell: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何やってるの？：
日付の比較とは、２つの日付を見て、どちらが前後するかを決めることです。プログラマーがこの作業をするのは、日付処理がプログラムにとってとても重要だからです。

## やり方：
```Haskell
import Data.Time.Clock

-- 日付を比較する関数
compareDates :: UTCTime -> UTCTime -> Ordering

-- 例
date1 = UTCTime (fromGregorian 2020 10 1) 0
date2 = UTCTime (fromGregorian 2020 9 10) 0
compareDates date1 date2 -- GT (Greater Than)
```

## 深入り：
日付の比較は、歴史的にはとても難しい問題でした。しかし、Haskellを使えば簡単に解決できます。また、より高度な日付処理をするためのライブラリもいくつかあります。また、日付オブジェクトにはより詳しい情報があり、異なるタイムゾーンや夏時間の扱い方についても理解する必要があります。

## 関連情報：
- [Haskell公式ドキュメント](https://www.haskell.org/)
- [Hackage - 日付処理についてのライブラリ](https://hackage.haskell.org/packages/search?terms=date)