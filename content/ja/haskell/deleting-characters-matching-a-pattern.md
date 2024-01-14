---
title:                "Haskell: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ

あるパターンに一致する文字を削除することについて考える方がいると思います。それは、文字列から特定の文字を除外したい場合や、文字列から特定のパターンを除外したい場合に役立ちます。

# 方法

まず、「Data.Text」というHaskellのモジュールを読み込む必要があります。次に、文字列の変数を作成し、`deleteAll`関数を使用して、削除したい文字のパターンを指定します。

```Haskell
import Data.Text (Text)
import qualified Data.Text as T

myString :: Text
myString = "こんにちは、世界"

deleteAll :: Char -> Text -> Text
deleteAll char = T.filter (/= char)

deleteAll 'ん' myString -- "こんにちは、世界"
```
上記の例では、文字列`"こんにちは、世界"`から文字`'ん'`を削除しました。同じ方法で、さまざまなパターンの文字を削除することができます。

また、`deleteAll`関数を`Data.List`モジュールを使って以下のように定義することもできます。

```Haskell
import Data.List (delete)

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll = delete
```

どちらの定義方法でも、同じ結果が得られます。

# 深堀り

上記の例では、文字列を操作するために`Data.Text`と`Data.List`を使用しました。`Data.Text`は、`String`と比べてより効率的な文字列の表現を提供します。`Data.List`は、リストを操作するための便利な関数をたくさん提供しています。

また、`deleteAll`関数を定義する際に使用した高階関数`(=)`は、プレディケートを引数にとり、与えられた引数と等しいものをフィルタリングする関数です。このように、Haskellでは高階関数を積極的に用いることができます。

# 関連記事

- [Haskellの公式ドキュメント](https://www.haskell.org/documentation/)
- [Haskell Wiki](https://wiki.haskell.org/)
- [Real World Haskell](http://book.realworldhaskell.org/read/)