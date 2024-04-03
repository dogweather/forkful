---
date: 2024-01-20 17:42:20.504269-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.160134-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to: (方法)
```Haskell
import Data.List (isInfixOf)
import Data.Char (isSpace)

-- パターンにマッチする文字を削除するシンプルな関数
deletePattern :: String -> String -> String
deletePattern pattern = unwords . filter (not . isInfixOf pattern) . words

-- 空白文字を削除する関数
deleteSpaces :: String -> String
deleteSpaces = filter (not . isSpace)

main :: IO ()
main = do
  -- パターンに"foo"が含まれる単語を削除
  putStrLn $ deletePattern "foo" "foobar baz fooqux quux"

  -- 空白を削除
  putStrLn $ deleteSpaces "He who controls the spice, controls the universe."
  
-- 出力
-- baz quux
-- Hewhocontrolsthespice,controlstheuniverse.
```

## Deep Dive (詳細)
この問題はテキスト処理の分野で基本となります。Haskellでは文字列処理を関数型の視点から取り組むことができます。例えば、`filter` 関数やリスト内包表記はパターンにマッチする要素の選択や除去に使えます。

歴史的に見ると、Haskellのテキスト処理能力は、Unixのツール、たとえば `sed` や `awk` に触発されたものです。しかしHaskellの関数型の特徴により、より高レベルで抽象的な操作が可能になります。

削除アルゴリズムには単純な `filter` から正規表現ライブラリまで、選択肢があります。例えば `regex-tdfa` パッケージはより複雑なパターンマッチングを提供しますが、ここでは扱っていません。

## See Also (参照)
- [Hackage: regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)
