---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結とは、複数の文字列を一つにまとめる操作を指す。文字列の組み合わせや改行追加など、データの整形を容易にするためにプログラマーは文字列を連結する。

## 手順

ここでは、Gleamでの文字列連結の例を示します。

```Gleam
let str1 = "こんにちは"
let str2 = "、"
let str3 = "世界"
let full_str = str1 ++ str2 ++ str3
io.println(full_str) //--> "こんにちは、世界"
```

このプログラムでは、3つの文字列を `++` 演算子を使って結合し、結果を表示しています。

## 深掘り

文字列を連結するというのは、プログラミングの歴史の中で常に使われてきた手法です。これには代替手段もあります、如何に例えば、`interp`関数を使うことでマークと値を組み合わせるなどがあります。しかし、Gleamでは基本的に文字列を連結する際には `++` 演算子を使います。これは、GleamがErlang VM上で動作し、Erlangではリスト（この場合、文字列）の連結が `++` 演算子で行われるためです。 

## 参考リンク

- [Gleamの文字列操作](https://gleam.run/book/tour/strings.html): Gleamでの他の文字列操作についての情報。