---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の長さを求めるとは、文字列の中に含まれる文字数を数えることです。この操作は、特定の文字を探したり、与えられたデータの特性を理解したりするためによく行われます。

## 使い方：

以下のように `length` メソッドを使って文字列の長さを取得しましょう：

```Ruby 
str = "こんにちは、世界！"
puts str.length # ブロックの結果を出力します
```

このコードを実行すると、「こんにちは、世界！」という文字列の長さ、つまり文字数が出力されます。

## 深掘り

文字列の長さを見つける概念は、コンピュータプログラミングが生まれた当初から存在しています。Rubyは1995年に誕生し、その以降、あらゆる言語でこの基本的な操作が提供されています。

代替手段として `size` メソッドも存在します。このメソッドも文字列の長さを返しますが、 `length` メソッドと同等の機能を持つため、主にプログラマーの好みによります。

文字列の長さを求めるメソッドは、Rubyの内部で文字列を一文字ずつ確認し、カウントを進めます。バイト単位で文字列の長さを求める `bytesize` メソッドもありますが、一般的な用途では `length` または `size` メソッドが推奨されます。

## 参照元

- [Ruby Documentation: String](https://docs.ruby-lang.org/ja/latest/class/String.html): 文字列に関する全面的な情報と所属するメソッドを提供します。

- [Stack Overflow: Difference between length and size](https://stackoverflow.com/questions/6083219/): `length` と `size` の違いについての議論を提供します。

- [How To Ruby: String Length](https://howtoruby.com/string-length/): Rubyでの文字列の長さの照会方法を解説します。