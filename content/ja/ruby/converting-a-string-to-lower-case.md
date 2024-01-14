---
title:    "Ruby: 文字列を小文字に変換する"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## なぜ

Rubyプログラミングをしていると、文字列を小文字に変換する必要がある場面がよくあります。例えば、ユーザーからの入力を変換してデータとして扱う場合や、文字列の比較を行う際には一般的に小文字に統一して扱います。この記事では、文字列を小文字に変換する方法を紹介します。

## 方法

文字列を小文字に変換するには、Rubyの組み込みメソッドである`downcase`を使用します。`downcase`は文字列を小文字に変換した新しい文字列を返します。使用方法は以下のとおりです。

```ruby
string = "Hello, WORLD!"
new_string = string.downcase

puts new_string #=> "hello, world!"
```

`downcase`を使用することで、元の文字列を変更せずに小文字に変換することができます。また、文字列の一部だけを小文字に変換したい場合は、`downcase`の引数に対象の範囲を指定することもできます。例えば、最初の文字だけを大文字にして残りは小文字に変換したい場合は以下のように書きます。

```ruby
string = "Hello, WORLD!"
new_string = string.downcase(0..0) + string.downcase(1..-1)

puts new_string #=> "Helloworld!"
```

## 深堀り

`downcase`メソッドは空白や数字、記号などの文字にも影響を与えます。例えば、フルネームを小文字にしたい場合、`downcase`だけを使用すると空白で区切られた名前ごとに小文字になります。そのため、各名前の先頭文字だけを大文字にするメソッドである`capitalize`と組み合わせる必要があります。

```ruby
name = "JAMES PATRICK SMITH"
new_name = name.downcase.split.map(&:capitalize).join(' ')

puts new_name #=> "James Patrick Smith"
```

また、`downcase`の代わりに`swapcase`メソッドを使用することで、文字を大文字と小文字を入れ替えることもできます。

```ruby
string = "HeLlO, WoRlD!"
new_string = string.swapcase

puts new_string #=> "hElLo, wOrLd!"
```

## お役立ちリンク

- [Rubyの公式ドキュメント：String#downcase](https://docs.ruby-lang.org/ja/latest/method/String/i/downcase.html)
- [Ruby on Railsガイド：文字列操作](https://railsguides.jp/active_support_core_extensions.html#%E6%96%87%E5%AD%97%E5%88%97%E6%93%8D%E4%BD%9C)