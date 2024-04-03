---
date: 2024-01-20 17:36:42.666800-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u306F\u3001\u65E5\u4ED8\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u3084\u3059\u3044\u30C6\
  \u30AD\u30B9\u30C8\u5F62\u5F0F\u3067\u8868\u73FE\u3059\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30ED\u30B0\u8A18\u9332\u3001\u30E6\
  \u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3001\u30C7\u30FC\
  \u30BF\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u6574\u7406\u306B\u305D\u306E\u64CD\
  \u4F5C\u3092\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.201784-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u306F\u3001\u65E5\u4ED8\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u3084\u3059\u3044\u30C6\
  \u30AD\u30B9\u30C8\u5F62\u5F0F\u3067\u8868\u73FE\u3059\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30ED\u30B0\u8A18\u9332\u3001\u30E6\
  \u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3001\u30C7\u30FC\
  \u30BF\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u6574\u7406\u306B\u305D\u306E\u64CD\
  \u4F5C\u3092\u4F7F\u3044\u307E\u3059\u3002."
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (方法)
```Haskell
import Data.Time

-- 日付を文字列に変換する例
convertDateToString :: IO String
convertDateToString = do
    current <- getCurrentTime
    return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" current
```

実行例:
```
> convertDateToString
"2023-04-03 15:26:37"
```

## Deep Dive (深イイ掘り)
日付の文字列変換は、UNIX時代からの一般的な処理です。`formatTime`関数はGHCUの`Data.Time`モジュールに含まれており、日付を多様なフォーマットで扱うことができます。代替方法として`time`ライブラリの他の関数や`old-time`ライブラリがありますが、現代のHaskellでは`Data.Time`が好まれます。`formatTime`の実装はロケールに依存し、時間帯や言語設定を反映することができます。

## See Also (関連情報)
- [Hackage Data.Time module](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
