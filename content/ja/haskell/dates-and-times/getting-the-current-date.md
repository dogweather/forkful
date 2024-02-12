---
title:                "現在の日付の取得"
aliases:
- /ja/haskell/getting-the-current-date/
date:                  2024-02-03T19:09:44.484369-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
