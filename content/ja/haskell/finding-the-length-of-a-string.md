---
title:    "Haskell: 文字列の長さを求める"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

日本の読者の皆さんこんにちは！今日は、ハスケルプログラミングの楽しみをお届けします。この記事では、文字列の長さを見つける方法について、Why、How To、Deep Diveの3つのセクションに分けて説明します。

## Why
文字列の長さを見つけることは、プログラミングで非常に一般的なタスクです。例えば、テキストメッセージの文字数を制限したり、入力された文字が指定された長さを超えるかどうかを判断する際に必要になります。そのため、文字列の長さを見つけることは、覚えておくべき重要なスキルです。

## How To
文字列の長さを見つける方法を学ぶために、まずは基本的なコードを使って実践してみましょう。下記のコードを見てください。

```Haskell
stringLength :: String -> Int 
stringLength "" = 0 
stringLength (_:xs) = 1 + stringLength xs 
```
このコードでは、`stringLength`関数が定義されています。この関数は、文字列を受け取り、その長さを返します。まず、空の文字列が与えられた場合、長さは0になります。それ以外の場合は、リストの最初の要素を除いた残りの文字列（`xs`）の長さに1を足します。具体的には、再帰的に呼び出し、最初の要素を除いた残りの文字列の長さを計算していることになります。これを繰り返すことで、最終的に文字列の長さを求めることができます。

下記の例では、`"Hello World"`という文字列を`stringLength`関数に渡しています。

```Haskell
stringLength "Hello World"
```
このコードを実行すると、以下のような出力が得られます。

```Haskell
11
```

ここでは、文字列の長さが11であることがわかりました。うまく動作しているようですね！

## Deep Dive
では、これからもっと深く`stringLength`関数を掘り下げていきましょう。

上記のコードでは、`++`演算子を使って文字列を結合しましたが、`String`型は実はリストとして定義されています。そのため、文字列の長さを調べることは、リストの長さを調べることと同じだということです。Haskellでは、リストの長さを調べる関数`length`が標準で定義されています。それを使って、`stringLength`関数を以下のように書き直すことができます。

```Haskell
stringLength :: String -> Int 
stringLength str = length str
```

このように、Haskellでは標準で便利な関数が多数定義されているため、コードをより簡潔に書くことができます。プログラミング初心者の方には、ぜひHaskellでの実践をお勧めします！

## See Also
- [Haskell Programming Language](https://www.haskell.org/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)

それでは、今日の記事があなたのハスケルプログラミングの学習に役立ちますように！ありがとうございました。