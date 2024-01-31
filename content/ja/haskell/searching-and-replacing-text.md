---
title:                "テキストの検索と置換"
date:                  2024-01-20T17:58:02.688168-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

category:             "Haskell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/searching-and-replacing-text.md"
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
