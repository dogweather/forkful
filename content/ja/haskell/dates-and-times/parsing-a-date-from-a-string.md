---
title:                "文字列から日付をパースする"
aliases:
- /ja/haskell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:42.584334-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Haskellで文字列から日付を解析することは、日付のテキスト表現をプログラムが操作できる構造化された形式に変換することを意味します。このプロセスは、カレンダーデータを扱うアプリケーションにとって基本的であり、期間の計算、スケジューリング、データの検証などの機能を可能にします。

## どのようにして：

Haskellは、箱から出してすぐに日付を解析するための基本的なツールを提供していますが、`time`ライブラリをコア機能として使ったり、より柔軟な解析のために`date-parse`や`time-parse`のようなライブラリを活用することで、この作業を大幅に簡素化できます。

まず、`time`ライブラリが利用可能であることを確認してください。それはしばしばGHCに含まれていますが、依存関係として指定する必要がある場合は、プロジェクトのcabalファイルに`time`を追加するか、`cabal install time`を使用して手動でインストールしてください。

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- time ライブラリを使って標準フォーマットの日付を解析する
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

使用例と出力：

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- 出力: Just 2023-03-31 22:00:00 UTC
```

複数のフォーマットやロケールを扱う必要があるより複雑なシナリオの場合、`date-parse`のようなサードパーティライブラリを使う方が便利です：

`date-parse`を依存関係に追加してインストールしたと仮定すると、ここに使用例があります：

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- date-parse ライブラリを使って日付文字列を解析すると、複数のフォーマットをサポートする
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

`date-parse`を使った使用例：

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- 出力: Just 2023-04-01
```

それぞれの例は、文字列を取り、それをHaskellで使える日付オブジェクトに変換する基本的なアプローチを示しています。`time`ライブラリの組み込み機能を使うか、`date-parse`のようなサードパーティの解決策を選ぶかは、扱う必要のある入力フォーマットの範囲など、アプリケーションの特定のニーズに依存します。
