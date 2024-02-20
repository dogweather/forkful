---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:42.584334-07:00
description: "Haskell\u3067\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\
  \u3059\u308B\u3053\u3068\u306F\u3001\u65E5\u4ED8\u306E\u30C6\u30AD\u30B9\u30C8\u8868\
  \u73FE\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u64CD\u4F5C\u3067\u304D\u308B\u69CB\
  \u9020\u5316\u3055\u308C\u305F\u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\
  \u3001\u30AB\u30EC\u30F3\u30C0\u30FC\u30C7\u30FC\u30BF\u3092\u6271\u3046\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\u57FA\u672C\u7684\u3067\
  \u3042\u308A\u3001\u671F\u9593\u306E\u8A08\u7B97\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\
  \u30EA\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u306E\u691C\u8A3C\u306A\u3069\u306E\u6A5F\
  \u80FD\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.342276
model: gpt-4-0125-preview
summary: "Haskell\u3067\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\
  \u3059\u308B\u3053\u3068\u306F\u3001\u65E5\u4ED8\u306E\u30C6\u30AD\u30B9\u30C8\u8868\
  \u73FE\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u64CD\u4F5C\u3067\u304D\u308B\u69CB\
  \u9020\u5316\u3055\u308C\u305F\u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\
  \u3001\u30AB\u30EC\u30F3\u30C0\u30FC\u30C7\u30FC\u30BF\u3092\u6271\u3046\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\u57FA\u672C\u7684\u3067\
  \u3042\u308A\u3001\u671F\u9593\u306E\u8A08\u7B97\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\
  \u30EA\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u306E\u691C\u8A3C\u306A\u3069\u306E\u6A5F\
  \u80FD\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
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
