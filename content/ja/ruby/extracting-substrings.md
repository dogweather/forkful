---
title:                "Ruby: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ？

サブストリングを抽出することのメリットはたくさんあります。例えば、大きな文字列から特定の部分を取り出したい場合や、特定の文字列を検索して置換する場合に役立ちます。また、文字列の処理を行う際にも必要不可欠です。サブストリングを抽出することは、より効率的なプログラミングを行うための重要なスキルです。

## 使い方

サブストリングを抽出する方法はいくつかありますが、ここではRubyのStringクラスのメソッドである`#slice`を使用する方法を紹介します。まずは以下の例をご覧ください。

```ruby
str = "こんにちは、世界！"

# 文字列の先頭から3文字目までを抽出
p str.slice(0,3) #=> "こんにちは"

# 3文字目以降を抽出
p str.slice(3..-1) #=> "、世界！"
```

`#slice`メソッドは、引数として抽出したい文字列の範囲を指定します。上の例では、0から3文字目までを抽出するように指定しています。また、`#slice`メソッドでは、文字列の一部を置き換えることもできます。

```ruby
str = "Hello, Ruby!"

# HelloをGoodbyeに置き換える
p str.slice!(0..4) #=> "Hello"
p str #=> "Goodbye, Ruby!"
```

上記の例では、`#slice!`メソッドを使用してHelloを抽出し、代わりにGoodbyeに置き換えています。

## 深堀り

Rubyでは、`#slice`メソッド以外にもサブストリングを抽出するための様々なメソッドが用意されています。例えば、`#slice!`メソッド以外にも`#partition`や`#scan`メソッドなどを使用することができます。また、正規表現を使用して任意の文字列を検索したり、変換したりすることもできます。

サブストリングを抽出する方法は数多くありますので、自分のプログラムに最適なメソッドを選択してください。

## 参考リンク

- [RubyのStringクラスについて](https://docs.ruby-lang.org/ja/latest/class/String.html)
- [Rubyで文字列を操作する方法](https://qiita.com/kazukimatsumoto/items/0d352e388f3973b3b272)
- [Rubyにおけるサブストリングの抽出について](https://morizyun.github.io/blog/ruby-string-cut-sample)