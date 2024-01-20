---
title:                "文字列を大文字にする"
html_title:           "Haskell: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を大文字化するとは、文字列内のすべての文字を大文字にすることを指します。これは、プログラマがユーザーの入力を標準化したり、アルファベットの大文字と小文字の区別を排除したりするために行います。

## どのように？

Haskellでは`map` 関数と `toUpper` 関数を組み合わせて、文字列を大文字にできます：

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper
```

例えば、`capitalize "hello world"` を実行すると、`"HELLO WORLD"`が出力されます。

## より深く

**歴史的背景**
Haskellの `toUpper`は、ASCIIとUnicodeの両方の文字を大文字に変換できます。この機能は、全世界のプログラマが様々な言語でコードを書く現代の多様な環境を反映しています。

**代替方法**
他の方法もあります。例えば、リスト内包を使用して同じ結果を得ることもできます：

```Haskell
capitalize2 :: String -> String
capitalize2 str = [toUpper ch | ch <- str]
```

**実装の詳細**
`toUpper` 関数は、Haskellの `Data.Char` モジュールに定義されています。そして `toUpper` は特定の文字を大文字の同等物に変換します。また、`map` 関数は各文字列の文字に `toUpper` 関数を適用します。

## 参考 

作成された大文字の文字列を小文字に変換する方法については、Haskellの `Data.Char` モジュールの `toLower` 関数を参照してください。

Haskellのリスト内包についての詳細は、公式のHaskellチュートリアル (https://www.haskell.org/tutorial/listcomps.html) をご覧ください。