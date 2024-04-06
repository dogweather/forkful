---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:42.584334-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Haskell\u306F\u3001\
  \u7BB1\u304B\u3089\u51FA\u3057\u3066\u3059\u3050\u306B\u65E5\u4ED8\u3092\u89E3\u6790\
  \u3059\u308B\u305F\u3081\u306E\u57FA\u672C\u7684\u306A\u30C4\u30FC\u30EB\u3092\u63D0\
  \u4F9B\u3057\u3066\u3044\u307E\u3059\u304C\u3001`time`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u30B3\u30A2\u6A5F\u80FD\u3068\u3057\u3066\u4F7F\u3063\u305F\u308A\u3001\u3088\
  \u308A\u67D4\u8EDF\u306A\u89E3\u6790\u306E\u305F\u3081\u306B`date-parse`\u3084`time-parse`\u306E\
  \u3088\u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u6D3B\u7528\u3059\u308B\u3053\
  \u3068\u3067\u3001\u3053\u306E\u4F5C\u696D\u3092\u5927\u5E45\u306B\u7C21\u7D20\u5316\
  \u3067\u304D\u307E\u3059\u3002\u2026"
lastmod: '2024-04-05T22:38:41.734406-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Haskell\u306F\u3001\u7BB1\
  \u304B\u3089\u51FA\u3057\u3066\u3059\u3050\u306B\u65E5\u4ED8\u3092\u89E3\u6790\u3059\
  \u308B\u305F\u3081\u306E\u57FA\u672C\u7684\u306A\u30C4\u30FC\u30EB\u3092\u63D0\u4F9B\
  \u3057\u3066\u3044\u307E\u3059\u304C\u3001`time`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u30B3\u30A2\u6A5F\u80FD\u3068\u3057\u3066\u4F7F\u3063\u305F\u308A\u3001\u3088\u308A\
  \u67D4\u8EDF\u306A\u89E3\u6790\u306E\u305F\u3081\u306B`date-parse`\u3084`time-parse`\u306E\
  \u3088\u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u6D3B\u7528\u3059\u308B\u3053\
  \u3068\u3067\u3001\u3053\u306E\u4F5C\u696D\u3092\u5927\u5E45\u306B\u7C21\u7D20\u5316\
  \u3067\u304D\u307E\u3059\u3002 \u307E\u305A\u3001`time`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u304C\u5229\u7528\u53EF\u80FD\u3067\u3042\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\
  \u3066\u304F\u3060\u3055\u3044\u3002\u305D\u308C\u306F\u3057\u3070\u3057\u3070GHC\u306B\
  \u542B\u307E\u308C\u3066\u3044\u307E\u3059\u304C\u3001\u4F9D\u5B58\u95A2\u4FC2\u3068\
  \u3057\u3066\u6307\u5B9A\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\u5834\u5408\u306F\
  \u3001\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306Ecabal\u30D5\u30A1\u30A4\u30EB\u306B\
  `time`\u3092\u8FFD\u52A0\u3059\u308B\u304B\u3001`cabal install time`\u3092\u4F7F\
  \u7528\u3057\u3066\u624B\u52D5\u3067\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u3066\
  \u304F\u3060\u3055\u3044\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
