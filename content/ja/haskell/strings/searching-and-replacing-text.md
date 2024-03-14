---
date: 2024-01-20 17:58:02.688168-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u7279\
  \u5B9A\u306E\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u5225\u306E\u6587\u5B57\
  \u5217\u306B\u5909\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u30B3\u30FC\u30C9\u5185\u306E\u30C7\u30FC\u30BF\u3092\u66F4\u65B0\
  \u3001\u4FEE\u6B63\u3001\u6216\u3044\u306F\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.161496-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u7279\
  \u5B9A\u306E\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u5225\u306E\u6587\u5B57\
  \u5217\u306B\u5909\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u30B3\u30FC\u30C9\u5185\u306E\u30C7\u30FC\u30BF\u3092\u66F4\u65B0\
  \u3001\u4FEE\u6B63\u3001\u6216\u3044\u306F\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ?
テキスト検索と置換は、特定の文字列を見つけて別の文字列に変えることです。プログラマーはコード内のデータを更新、修正、或いはリファクタリングするためにこれを行います。

## How to: / 方法
```Haskell
import Data.Text as T

-- 文字列を検索して置換する関数
searchReplace :: Text -> Text -> Text -> Text
searchReplace old new = T.replace old new

-- 使用例
main :: IO ()
main = do
  let text = "こんにちは、世界！"
  let searchText = "世界"
  let replaceText = "Haskell"
  putStrLn $ T.unpack (searchReplace searchText replaceText text)
```

出力:
```
こんにちは、Haskell！
```

## Deep Dive / 深堀り
検索と置換機能は、古くからテキストエディタやワードプロセッサに存在します。Haskellでの置換は純粋関数的で、副作用が発生しない。`Data.Text`は`String`よりも効率的な操作を提供する。他にも`regex-tdfa`などの正規表現ライブラリが置換に使える。`Data.Text`の`replace`は素直だが、複雑なパターンには正規表現が適している。

## See Also / 関連情報
- [Hackage `text` package](https://hackage.haskell.org/package/text)
- [Hackage `regex-tdfa` package](https://hackage.haskell.org/package/regex-tdfa)

これらのリンクは、Haskellでのテキスト操作に関するより深い理解に繋がるでしょう。`text`パッケージのドキュメントは、`Data.Text`モジュールの各関数の使用方法を調べるのに役立ちます。また、正規表現に関する資料は複雑なテキスト操作についての理解を深めるのに有用です。
