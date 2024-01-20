---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の長さを求めるとは、文字列が含む文字の数を計算することを指します。プログラマーは、メモリ使用量を把握するため、または特定のアルゴリズムが適用可能かどうかを判断するために、この操作を実行します。

## 方法:

`length`関数を使用して、Haskellで文字列の長さを簡単に取得できます。

```Haskell
myString = "こんにちは、世界"
main = print (length myString)
```

実行結果は次のとおりです:

```Haskell
8
```

## ディープダイブ:

歴史的な文脈：Haskellの文字列は、文字のリストとして実装されています。`length`関数は、リストの長さを返す一般的な関数で、文字列での使用は特別な事例ではありません。

代替手段：リストの長さを取得する他の方法として、`foldl'`関数を使う方法もあります。しかし、`length`関数の方が直感的で、より効率的です。

実装詳細：`length`関数は、再帰を使用してリスト全体を走査します。再帰呼び出しごとにカウンターが1増加し、リストの終わりに到達すると、その数が返されます。

## 参考文献:

1. [Learn You a Haskell for Great Good - Strings](http://learnyouahaskell.com/starting-out#strings)
2. [Real World Haskell - Lists and Tuples](http://book.realworldhaskell.org/read/lists-and-tuples.html)
3. [Haskell Documentation - Data.List.length](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:length)