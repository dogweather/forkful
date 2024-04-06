---
date: 2024-01-20 17:58:02.688168-07:00
description: "How to: / \u65B9\u6CD5 \u691C\u7D22\u3068\u7F6E\u63DB\u6A5F\u80FD\u306F\
  \u3001\u53E4\u304F\u304B\u3089\u30C6\u30AD\u30B9\u30C8\u30A8\u30C7\u30A3\u30BF\u3084\
  \u30EF\u30FC\u30C9\u30D7\u30ED\u30BB\u30C3\u30B5\u306B\u5B58\u5728\u3057\u307E\u3059\
  \u3002Haskell\u3067\u306E\u7F6E\u63DB\u306F\u7D14\u7C8B\u95A2\u6570\u7684\u3067\u3001\
  \u526F\u4F5C\u7528\u304C\u767A\u751F\u3057\u306A\u3044\u3002`Data.Text`\u306F`String`\u3088\
  \u308A\u3082\u52B9\u7387\u7684\u306A\u64CD\u4F5C\u3092\u63D0\u4F9B\u3059\u308B\u3002\
  \u4ED6\u306B\u3082`regex-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.032579-06:00'
model: gpt-4-1106-preview
summary: "/ \u65B9\u6CD5 \u691C\u7D22\u3068\u7F6E\u63DB\u6A5F\u80FD\u306F\u3001\u53E4\
  \u304F\u304B\u3089\u30C6\u30AD\u30B9\u30C8\u30A8\u30C7\u30A3\u30BF\u3084\u30EF\u30FC\
  \u30C9\u30D7\u30ED\u30BB\u30C3\u30B5\u306B\u5B58\u5728\u3057\u307E\u3059\u3002Haskell\u3067\
  \u306E\u7F6E\u63DB\u306F\u7D14\u7C8B\u95A2\u6570\u7684\u3067\u3001\u526F\u4F5C\u7528\
  \u304C\u767A\u751F\u3057\u306A\u3044\u3002`Data.Text`\u306F`String`\u3088\u308A\u3082\
  \u52B9\u7387\u7684\u306A\u64CD\u4F5C\u3092\u63D0\u4F9B\u3059\u308B\u3002\u4ED6\u306B\
  \u3082`regex-tdfa`\u306A\u3069\u306E\u6B63\u898F\u8868\u73FE\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u304C\u7F6E\u63DB\u306B\u4F7F\u3048\u308B\u3002`Data.Text`\u306E`replace`\u306F\
  \u7D20\u76F4\u3060\u304C\u3001\u8907\u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u306B\u306F\
  \u6B63\u898F\u8868\u73FE\u304C\u9069\u3057\u3066\u3044\u308B\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

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
