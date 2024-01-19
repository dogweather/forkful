---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## なにそれ？そして、なぜ？

文字列の連結とは、２つ以上の文字列を１つにまとめるプロセスを指します。プログラマーは、より複雑な文章を構築したり、さまざまなやり方で出力をカスタマイズしたりするためにこれを行います。

## どうやって：

文字列の連結はシンプルです。 "+" 演算子または "<< " 演算子を使用して2つの文字列を連結できます。

```Ruby
str1 = "こんにちは、"
str2 = "世界！"
puts str1 + str2
```

出力：

```Ruby
こんにちは、世界！
```

または、

```Ruby
str1 = "こんにちは、"
str2 = "世界！"
str1 << str2
puts str1
```

出力：

```Ruby
こんにちは、世界！
```

## ディープダイブ：

文字列の連結は古典的な概念で、Rubyが存在する以前から実装されていました。Rubyでは、"<< "は文字列連結演算子として使用され、配列操作やビットシフト操作でも使用されます。

なお、Rubyには文字列の結合を行う他の方法もあります。例えば、"join"メソッドを使う方法があります。

```Ruby
arr = ["こんにちは、", "世界！"]
puts arr.join(' ')
```

Rubyの文字列連結は、新しい文字列を作成せずに元の文字列に追加する点でユニークです。これはメモリ管理に非常に効率的です。

## 参照：

1. Ruby Documentation: [String Concatenation](https://ruby-doc.org/core-2.6.1/String.html#method-i-2B)
2. Ruby Guides: [Understanding String Interpolation in Ruby](https://www.rubyguides.com/2018/01/ruby-string-methods/)
3. Ruby Monstas: [String Concatenation and Interpolation](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings_concatenation.html)

これらのリソースを使用して、文字列の連結と操作についてさらに理解を深めてください！