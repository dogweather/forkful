---
title:                "Haskell: 文字列の切り出し"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ？

文字列から部分文字列を抽出することは、Haskellプログラミングにおいて重要なタスクの一つです。例えば、文字列から特定の単語を抽出したり、特定の文字列に対する処理を行ったりする際に使用されます。この記事では、Haskellにおける部分文字列の抽出について解説します。

## 方法

まず、部分文字列を抽出するためには、`take`関数や`drop`関数を使用します。例えば、`take 5 "Hello World"`を実行すると、結果として"Hello"という部分文字列が抽出されます。また、`drop 5 "Hello World"`を実行すると、結果として" World"という部分文字列が抽出されます。

```Haskell
take 5 "Hello World" -- "Hello"
drop 5 "Hello World" -- " World"
```

さらに、`splitAt`関数を使用することで、任意の位置で文字列を分割することができます。例えば、`splitAt 5 "Hello World"`を実行すると、結果として("Hello", "World")というタプルが返されます。

```Haskell
splitAt 5 "Hello World" -- ("Hello", "World")
```

さらに、`words`関数を使用することで、文字列をスペースで分割し、部分文字列のリストを取得することができます。また、`unwords`関数を使用することで、リストをスペースで結合して文字列に変換することもできます。

```Haskell
words "Hello World" -- ["Hello", "World"]
unwords ["Hello", "World"] -- "Hello World"
```

## ディープダイブ

部分文字列を抽出する際には、パターンマッチングやリスト内包表記を使用することもできます。例えば、特定の文字列が含まれる部分文字列を抽出する方法は以下のようになります。

```Haskell
-- パターンマッチングを使用した場合
extractSubstrings :: String -> [String]
extractSubstrings "" = []
extractSubstrings str@(c:cs)
  | "H" `isPrefixOf` str = "H" : extractSubstrings cs -- "H"が先頭にある場合
  | "ello" `isPrefixOf` str = "ello" : extractSubstrings cs -- "ello"が先頭にある場合
  | otherwise = extractSubstrings cs -- 上記のどれにも当てはまらない場合

-- リスト内包表記を使用した場合
extractSubstrings :: String -> [String]
extractSubstrings str = [x | x <- ["H", "ello"], x `isPrefixOf` str]
```

また、正規表現を使用することで、より柔軟なパターンマッチングを行うこともできます。

```Haskell
import Text.Regex.Posix

extractSubstrings :: String -> [String]
extractSubstrings str = getAllTextMatches (str =~ "a*b" :: AllTextMatches [] String)
```

## 参考

- [Haskellで文字列を扱うための関数一覧](https://qiita.com/yuta-ushijima/items/409d9d3b8556dd482e4c)
- [Haskellの正規表現ライブラリRegexの基本](http://blog.masuidrive.jp/2012/05/haskellregex.html)
- [Haskell wiki: Take and Drop](https://wiki.haskell.org/Take_and_drop)
- [Haskell wiki: List comprehensions](https://wiki.haskell.org/List_comprehension)