---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u3068\u3044\
  \u3046\u306E\u306F\u3001\u901A\u5E38\u3001\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\
  \u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u3057\u304B\u3057\u3001\u6642\u306B\u306F\u6700\u521D\u306E\u6587\u5B57\u3092\
  \u5927\u6587\u5B57\u306B\u3057\u3066\u3001\u6B8B\u308A\u306E\u6587\u5B57\u5217\u3092\
  \u5909\u66F4\u3057\u306A\u3044\u307E\u307E\u306B\u3059\u308B\u3053\u3068\u3092\u610F\
  \u5473\u3059\u308B\u3053\u3068\u3082\u3042\u308A\u307E\u3059\u3002\u6B63\u76F4\u306A\
  \u3068\u3053\u308D\u3001\u79C1\u306E\u610F\u898B\u3067\u306F\u3001\u305D\u308C\u306F\
  \u3084\u3084\u66D6\u6627\u306A\u7528\u8A9E\u3067\u3059\u3002"
lastmod: '2024-03-25T19:22:13.850974-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u3068\u3044\
  \u3046\u306E\u306F\u3001\u901A\u5E38\u3001\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\
  \u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u3057\u304B\u3057\u3001\u6642\u306B\u306F\u6700\u521D\u306E\u6587\u5B57\u3092\
  \u5927\u6587\u5B57\u306B\u3057\u3066\u3001\u6B8B\u308A\u306E\u6587\u5B57\u5217\u3092\
  \u5909\u66F4\u3057\u306A\u3044\u307E\u307E\u306B\u3059\u308B\u3053\u3068\u3092\u610F\
  \u5473\u3059\u308B\u3053\u3068\u3082\u3042\u308A\u307E\u3059\u3002\u6B63\u76F4\u306A\
  \u3068\u3053\u308D\u3001\u79C1\u306E\u610F\u898B\u3067\u306F\u3001\u305D\u308C\u306F\
  \u3084\u3084\u66D6\u6627\u306A\u7528\u8A9E\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 何となく理由（What & Why?）
文字列を大文字にするというのは、通常、文字列の最初の文字を大文字にし、残りを小文字に変換することを意味します。しかし、時には最初の文字を大文字にして、残りの文字列を変更しないままにすることを意味することもあります。正直なところ、私の意見では、それはやや曖昧な用語です。

## 方法（How to:）
Rubyは、大文字化を含む、[文字列操作のための直感的な方法](https://docs.ruby-lang.org/en/3.3/String.html)を提供しています：

```ruby
# Rubyの組み込みメソッド
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

非常に便利です。

Rubyの`.capitalize`メソッドは便利ですが、最初の文字だけを大文字に変更します。より細かい制御をしたい場合や、文字列の各単語を大文字にする（タイトルケースとして知られている）ことが必要な場合は、Rails ActiveSupport拡張の`titleize`メソッドを使用するか、自分で実装したいかもしれません：

```ruby
# RailsのActiveSupportの'titleize'を使用
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# 自作の解決策
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

この方法は、文字列を単語の配列に分割し、それぞれを大文字にして、スペースで再び結合します。

個人的には、このアイデアを私のコードでさらに推し進めています。私は小さな単語、例えば「a」や「the」を考慮に入れた[`titleize`メソッドを自分で書きました](https://github.com/public-law/law_string/blob/master/lib/law_string.rb)。
