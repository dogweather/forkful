---
date: 2024-01-20 17:36:42.666800-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.201784-06:00'
model: gpt-4-1106-preview
summary: .
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
