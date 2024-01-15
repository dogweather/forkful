---
title:                "文字列の長さを見つける"
html_title:           "Haskell: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ文字列の長さを求めるか

文字列の長さを求めると、プログラムの機能を拡張することができます。例えば、ユーザーからの入力の長さを制限したり、文字列を分割する際の基準として利用したりすることができます。

## 使い方

文字列の長さを求めるためには、`length`関数を使用します。以下の例では、`str`という文字列の長さを求め、結果をコンソールに出力しています。

```Haskell
str = "Hello World!"
putStrLn ("Length of str: " ++ show (length str))
```

出力結果:

```
Length of str: 12
```

## 詳細を掘り下げる

`length`関数は、`String`型の引数を受け取り、その文字列の長さを返します。`String`型は、本質的に`[Char]`型のエイリアスです。つまり、文字列は文字のリストとして表現されます。

`length`関数は、再帰的アルゴリズムを使用して実装されています。つまり、与えられた文字列を1文字ずつチェックして、その数を数え上げるという方法を取っています。そのため、文字列の長さが長くなるほど計算量も増えていきます。

## 参考リンク

- [Haskellの公式ドキュメント](https://www.haskell.org/documentation/)
- [Haskell入門](https://qiita.com/tanakh/items/0ba42c7ca36cd29d0ac8)
- [文字列を操作するHaskell関数の一覧](https://wiki.haskell.org/Strings)