---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:44.484369-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Haskell\u306E\u6A19\u6E96\u30E9\
  \u30A4\u30D6\u30E9\u30EA`base`\u306B\u306F\u3001\u65E5\u4ED8\u3068\u6642\u523B\u3092\
  \u6271\u3046\u6A5F\u80FD\u3092\u63D0\u4F9B\u3059\u308B`Data.Time`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u305F\u3081\u306B\u305D\u308C\u3092\u4F7F\
  \u7528\u3059\u308B\u65B9\u6CD5\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\u3059\uFF1A\
  ."
lastmod: '2024-03-13T22:44:42.200264-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA`base`\u306B\u306F\
  \u3001\u65E5\u4ED8\u3068\u6642\u523B\u3092\u6271\u3046\u6A5F\u80FD\u3092\u63D0\u4F9B\
  \u3059\u308B`Data.Time`\u30E2\u30B8\u30E5\u30FC\u30EB\u304C\u542B\u307E\u308C\u3066\
  \u3044\u307E\u3059\u3002\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u305F\u3081\u306B\u305D\u308C\u3092\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u306F\u4EE5\
  \u4E0B\u306E\u901A\u308A\u3067\u3059\uFF1A."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## どのように：
Haskellの標準ライブラリ`base`には、日付と時刻を扱う機能を提供する`Data.Time`モジュールが含まれています。現在の日付を取得するためにそれを使用する方法は以下の通りです：

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

サンプル出力：
```
2023-04-12
```

日付の書式設定や異なるタイムゾーンでの操作など、より柔軟な機能が必要な場合には、`time`ライブラリが非常に価値があります。現在の日付をフォーマットする方法は以下の通りです：

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

これは、ローカルタイムゾーンに合わせて、`YYYY-MM-DD`形式で現在の日付を印刷します。

さらに、サードパーティのライブラリサポートについては、日付と時刻の操作機能が広範囲にわたるため、「time」ライブラリがHaskellコミュニティ内で広く推奨され、使用されています。上記の例は、このライブラリを利用しています。

文字列からの解析や日付と時刻に関する算術操作など、より包括的な日付操作が必要な場合は、`Data.Time`内の追加機能を探索すると良いでしょう。
