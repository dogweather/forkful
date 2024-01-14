---
title:    "Haskell: 文字列の大文字化"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字に変換することの意義について皆さんは考えたことがありますか？今日は、Haskellプログラミングで文字列を大文字に変換することの重要性についてお話ししましょう。

## 方法

まずは、文字列を大文字に変換する基本的な方法を見ていきましょう。以下のコードブロックを参考にしてください。

```Haskell
import Data.Char

capitalize :: String -> String
capitalize str = map toUpper str
```

このコードでは、`Data.Char`モジュールから`toUpper`関数を使用して、文字列を大文字に変換する`capitalize`関数を定義しています。そして、`map`関数を使用して、文字列の各文字に`toUpper`関数を適用し、大文字に変換しています。

例えば、`"hello world"`という文字列を`capitalize`関数に渡すと、`"HELLO WORLD"`という結果が得られます。

また、特定の文字だけを大文字にしたい場合は、以下のように`toUpper`関数を使うことができます。

```Haskell
capitalizeFirstLetter :: String -> String
capitalizeFirstLetter (x:xs) = (toUpper x):xs
```

この関数では、文字列の最初の文字を大文字に変換して返しています。例えば、`"hello world"`を渡すと、`"Hello world"`という結果が得られます。

## 深堀り

文字列を大文字に変換する方法を見てきましたが、その背景や仕組みについて深く理解することも重要です。Haskellでは、文字列は文字のリストとして表現されます。つまり、`"hello world"`は`['h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd']`というリストに変換されます。

そして、`map`関数はリストを受け取り、各要素に指定された関数を適用して新しいリストを返します。つまり、`map toUpper "hello"`は`"HELLO"`という結果が得られるのです。

また、`toUpper`関数はUnicode文字を扱うことができるため、多言語の文字列にも対応することができます。

## 参考

- [Haskell Wiki - Data.Char](https://wiki.haskell.org/Data.Char)
- [Learn You a Haskell for Great Good! - Type basics](http://learnyouahaskell.com/types-and-typeclasses#believe-the-type)
- [Programming in Haskell - Lists and Strings](http://www.cs.nott.ac.uk/~pszgmh/pih.html#listsstrings)