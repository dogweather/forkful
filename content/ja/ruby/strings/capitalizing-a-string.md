---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u65B9\u6CD5\uFF08How to:\uFF09 Ruby\u306F\u3001\u5927\u6587\u5B57\u5316\
  \u3092\u542B\u3080\u3001[\u6587\u5B57\u5217\u64CD\u4F5C\u306E\u305F\u3081\u306E\u76F4\
  \u611F\u7684\u306A\u65B9\u6CD5](https://docs.ruby-lang.org/en/3.3/String.html)\u3092\
  \u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\uFF1A."
lastmod: '2024-03-25T19:22:13.850974-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u306F\u3001\u5927\u6587\u5B57\u5316\u3092\u542B\u3080\u3001[\u6587\u5B57\
  \u5217\u64CD\u4F5C\u306E\u305F\u3081\u306E\u76F4\u611F\u7684\u306A\u65B9\u6CD5](https://docs.ruby-lang.org/en/3.3/String.html)\u3092\
  \u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

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
