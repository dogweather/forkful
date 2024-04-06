---
date: 2024-01-20 17:36:42.666800-07:00
description: "How to: (\u65B9\u6CD5) \u65E5\u4ED8\u306E\u6587\u5B57\u5217\u5909\u63DB\
  \u306F\u3001UNIX\u6642\u4EE3\u304B\u3089\u306E\u4E00\u822C\u7684\u306A\u51E6\u7406\
  \u3067\u3059\u3002`formatTime`\u95A2\u6570\u306FGHCU\u306E`Data.Time`\u30E2\u30B8\
  \u30E5\u30FC\u30EB\u306B\u542B\u307E\u308C\u3066\u304A\u308A\u3001\u65E5\u4ED8\u3092\
  \u591A\u69D8\u306A\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u6271\u3046\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u4EE3\u66FF\u65B9\u6CD5\u3068\u3057\u3066`time`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u306E\u4ED6\u306E\u95A2\u6570\u3084`old-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.064433-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u65E5\u4ED8\u306E\u6587\u5B57\u5217\u5909\u63DB\u306F\u3001\
  UNIX\u6642\u4EE3\u304B\u3089\u306E\u4E00\u822C\u7684\u306A\u51E6\u7406\u3067\u3059\
  \u3002`formatTime`\u95A2\u6570\u306FGHCU\u306E`Data.Time`\u30E2\u30B8\u30E5\u30FC\
  \u30EB\u306B\u542B\u307E\u308C\u3066\u304A\u308A\u3001\u65E5\u4ED8\u3092\u591A\u69D8\
  \u306A\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u6271\u3046\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002\u4EE3\u66FF\u65B9\u6CD5\u3068\u3057\u3066`time`\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306E\u4ED6\u306E\u95A2\u6570\u3084`old-time`\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u304C\u3042\u308A\u307E\u3059\u304C\u3001\u73FE\u4EE3\u306EHaskell\u3067\
  \u306F`Data.Time`\u304C\u597D\u307E\u308C\u307E\u3059\u3002`formatTime`\u306E\u5B9F\
  \u88C5\u306F\u30ED\u30B1\u30FC\u30EB\u306B\u4F9D\u5B58\u3057\u3001\u6642\u9593\u5E2F\
  \u3084\u8A00\u8A9E\u8A2D\u5B9A\u3092\u53CD\u6620\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002"
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
