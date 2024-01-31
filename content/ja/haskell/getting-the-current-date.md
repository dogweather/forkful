---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:14:40.231948-07:00
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラマが現在の日付を取得する理由: ログにタイムスタンプを追加したり、日付依存ではたらく関数やタスクを扱うためです。

## How to: (方法)
Haskellで現在の日付を取得するには`Data.Time`モジュールを使います。以下の例を見てください。

```Haskell
import Data.Time

main :: IO ()
main = do
  currentDate <- getCurrentTime
  print currentDate
```

実行した時の出力例:

```
2023-03-14 13:45:02.345 UTC
```

## Deep Dive (深堀り)
現在の日付と時刻を取得する機能は長い歴史を持つ。Haskellの`Data.Time`モジュールはそれを容易にし、広く使われている。過去には`old-time`パッケージもありましたが、今は非推奨です。`getCurrentTime`関数はUTC (協定世界時)を返しますが、タイムゾーン変換を行うこともできます。実装の詳細については、内部でシステムの時計と連携し、実行時の時刻を取得しています。

## See Also (関連リンク)
- [`Data.Time` モジュール](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [The `time` package on Hackage](https://hackage.haskell.org/package/time)
