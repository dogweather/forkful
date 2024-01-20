---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列からパターンに一致する文字を削除するとは、特定の文字列パターンを識別し、それを別のものに置換するか、または完全に削除することを指します。これは、不要なホワイトスペースを除去したり、特定の記号を取り除くためにプログラマーがよく使用します。

## 使い方

Rubyでは`gsub`メソッドを使用してこの操作を行うことができます。以下にサンプルコードと出力結果を示します：

```ruby
str = "H3llo, W0rld!"
str = str.gsub(/[0-9]/, '')
puts str
```

実行すると、

```
Hello, World!
```

と出力されます。上記のコードは、`str`内のすべての数字を削除しています。

## ディープダイブ

`gsub`メソッドは「global substitution」、つまり「全体にわたる置換」を意味します。文字列内のすべてのパターン一致を置換/削除します。

代替手段として、`sub`メソッドがあります。しかし、これは最初の一致しか削除しない点が異なります。

また、背後の実装の詳細について言えば、`gsub`は正規表現を利用しています。これにより、任意のパターン一致を識別・置換することが可能になります。

## 参考する

[データクリーニング](http://datascience.tokyotech.org/lectures/data-cleaning.html): データクリーニングにおける文字列操作の詳細。

[Ruby によるテキスト操作](https://programming-place.net/ppp/contents/ruby/008.html): Rubyでのテキスト操作、正規表現の利用方法に関する詳細。