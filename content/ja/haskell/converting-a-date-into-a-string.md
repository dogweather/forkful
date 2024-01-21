---
title:                "日付を文字列に変換する"
date:                  2024-01-20T17:36:42.666800-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換するとは、日付データを読みやすいテキスト形式で表現することです。プログラマーはログ記録、ユーザーインターフェイス、データのフォーマット整理にその操作を使います。

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