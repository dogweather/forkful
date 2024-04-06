---
date: 2024-01-20 17:31:11.255401-07:00
description: "How to: (\u65B9\u6CD5) \u6642\u9593\u3092\u6271\u3046\u8A08\u7B97\u306F\
  \u9577\u3044\u9593\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u57FA\u672C\u7684\
  \u306A\u90E8\u5206\u3067\u3059\u3002\u6B74\u53F2\u7684\u306B\u3001\u4F55\u3089\u304B\
  \u306E\u5F62\u3067\u65E5\u4ED8\u8A08\u7B97\u3092\u884C\u3046\u5FC5\u8981\u304C\u3042\
  \u308A\u307E\u3057\u305F\u3002Haskell \u306E `Data.Time` \u30E2\u30B8\u30E5\u30FC\
  \u30EB\u306F\u3001\u65E5\u4ED8\u3068\u6642\u523B\u3092\u6271\u3046\u305F\u3081\u306E\
  \u6A19\u6E96\u7684\u306A\u624B\u6BB5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002 \u4EE3\
  \u66FF\u624B\u6BB5\u3068\u3057\u3066\u306F\u3001`time` \u30E9\u30A4\u30D6\u30E9\u30EA\
  \u4EE5\u5916\u306B\u3082\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.740550-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6642\u9593\u3092\u6271\u3046\u8A08\u7B97\u306F\u9577\u3044\
  \u9593\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u57FA\u672C\u7684\u306A\u90E8\
  \u5206\u3067\u3059\u3002\u6B74\u53F2\u7684\u306B\u3001\u4F55\u3089\u304B\u306E\u5F62\
  \u3067\u65E5\u4ED8\u8A08\u7B97\u3092\u884C\u3046\u5FC5\u8981\u304C\u3042\u308A\u307E\
  \u3057\u305F\u3002Haskell \u306E `Data.Time` \u30E2\u30B8\u30E5\u30FC\u30EB\u306F\
  \u3001\u65E5\u4ED8\u3068\u6642\u523B\u3092\u6271\u3046\u305F\u3081\u306E\u6A19\u6E96\
  \u7684\u306A\u624B\u6BB5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

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
