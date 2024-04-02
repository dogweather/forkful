---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:44.484369-07:00
description: "Haskell\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u73FE\u5728\u6642\u523B\u3092\
  \u53D6\u5F97\u3057\u3001\u305D\u308C\u3092\u8AAD\u307F\u3084\u3059\u3044\u65E5\u4ED8\
  \u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u306E\u8A18\u9332\
  \u3001\u30BF\u30B9\u30AF\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u306E\u30A4\u30D9\u30F3\u30C8\
  \u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u306A\u3069\u3001\u65E5\u4ED8\u306B\
  \u57FA\u3065\u3044\u3066\u64CD\u4F5C\u3092\u884C\u3046\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.200264-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u73FE\u5728\u6642\u523B\u3092\
  \u53D6\u5F97\u3057\u3001\u305D\u308C\u3092\u8AAD\u307F\u3084\u3059\u3044\u65E5\u4ED8\
  \u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u306E\u8A18\u9332\
  \u3001\u30BF\u30B9\u30AF\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u306E\u30A4\u30D9\u30F3\u30C8\
  \u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u306A\u3069\u3001\u65E5\u4ED8\u306B\
  \u57FA\u3065\u3044\u3066\u64CD\u4F5C\u3092\u884C\u3046\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 何となぜ？
Haskellで現在の日付を取得することは、システムの現在時刻を取得し、それを読みやすい日付形式に変換することを含みます。プログラマーは、ログの記録、タスクのスケジューリング、アプリケーションでのイベントのタイムスタンプなど、日付に基づいて操作を行うためにこれを行います。

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
