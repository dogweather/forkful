---
title:                "将来または過去の日付を計算する"
aliases:
- /ja/haskell/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:11.255401-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
計算日付は未来または過去の特定日を求めること。プログラマは期限やスケジュールを管理するために使う。

## How to: (方法)
```Haskell
import Data.Time

-- 今日の日付を取得して、10日後の日付を計算する例
main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  let tenDaysLater = addDays 10 today
  putStrLn $ "10日後は: " ++ show tenDaysLater
```
実行結果:
```
10日後は: 2023-05-03
```

```Haskell
-- 特定の日付から60日前を計算する例
main :: IO ()
main = do
  let pastDate = fromGregorian 2023 3 25 -- 2023年3月25日
  let sixtyDaysEarlier = addDays (-60) pastDate
  putStrLn $ "60日前は: " ++ show sixtyDaysEarlier
```
実行結果:
```
60日前は: 2023-01-24
```

## Deep Dive (深い潜在)
時間を扱う計算は長い間プログラミングの基本的な部分です。歴史的に、何らかの形で日付計算を行う必要がありました。Haskell の `Data.Time` モジュールは、日付と時刻を扱うための標準的な手段を提供します。

代替手段としては、`time` ライブラリ以外にも `chronos`, `thyme` などのライブラリがありますが、`Data.Time`が一般的に推奨されています。

実装の詳細として、日付の計算は内部的にグレゴリオ暦を使用して、閏年などの規則にも対応しています。正確な計算には多くの時刻計算とタイムゾーンの扱いが含まれることがあります。

## See Also (関連情報)
- Haskell `Data.Time` ライブラリドキュメント： [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- `chronos` ライブラリ： [https://hackage.haskell.org/package/chronos](https://hackage.haskell.org/package/chronos)
- `thyme` ライブラリ： [https://hackage.haskell.org/package/thyme](https://hackage.haskell.org/package/thyme)
