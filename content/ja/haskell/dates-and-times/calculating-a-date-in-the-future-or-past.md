---
date: 2024-01-20 17:31:11.255401-07:00
description: "\u8A08\u7B97\u65E5\u4ED8\u306F\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\
  \u306E\u7279\u5B9A\u65E5\u3092\u6C42\u3081\u308B\u3053\u3068\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u671F\u9650\u3084\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\u3092\u7BA1\
  \u7406\u3059\u308B\u305F\u3081\u306B\u4F7F\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.775356-06:00'
model: gpt-4-1106-preview
summary: "\u8A08\u7B97\u65E5\u4ED8\u306F\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\
  \u306E\u7279\u5B9A\u65E5\u3092\u6C42\u3081\u308B\u3053\u3068\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u671F\u9650\u3084\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\u3092\u7BA1\
  \u7406\u3059\u308B\u305F\u3081\u306B\u4F7F\u3046\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
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
