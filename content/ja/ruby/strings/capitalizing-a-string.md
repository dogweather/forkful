---
title:                "文字列を大文字にする"
date:                  2024-03-25T17:31:54.882366-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列を大文字化するというのは、通常、文字列の最初の文字を大文字に、残りを小文字に変換することを意味します。しかし、時には、最初の文字を大文字にするだけで、文字列の残りを変更せずにそのままにすることも意味します。正直なところ、私の意見では、これはやや曖昧な用語です。

## 方法：
Rubyは、大文字化を含む[文字列操作のための直感的な方法](https://docs.ruby-lang.org/ja/3.3/String.html)を提供しています：

```ruby
# Rubyの組み込みメソッド
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

とても便利です。

Rubyの`.capitalize`メソッドは便利ですが、最初の文字のみを大文字にします。文字列の各単語を大文字化（タイトルケースとして知られる）するために、より多くのコントロールを得るために、RailsのActiveSupport拡張から`titleize`メソッドを使用するか、自分で実装することも考えられます：

```ruby
# RailsのActiveSupportから'titleize'を使用
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# 自作のソリューション
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

この方法は、文字列を単語の配列に分割し、各単語を大文字化し、それらをスペースで再度結合します。

個人的には、このアイデアを私のコードでずっと前に取り入れています。私は自分で[`titleize`メソッドを書いており、"a"や"the"のような小さな単語も考慮しています](https://github.com/public-law/law_string/blob/master/lib/law_string.rb)。
