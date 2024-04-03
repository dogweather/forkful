---
date: 2024-01-20 17:39:11.226599-07:00
description: "How to: (\u65B9\u6CD5) Ruby\u3067\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3059\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002`downcase`\u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u307E\u3059\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.833790-06:00'
model: gpt-4-1106-preview
summary: "Ruby\u3067\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\
  \u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002`downcase`\u30E1\u30BD\u30C3\u30C9\
  \u3092\u4F7F\u7528\u3057\u307E\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## How to: (方法)
Rubyで文字列を小文字に変換するのは簡単です。`downcase`メソッドを使用します：

```ruby
original_string = "Kon'nichiwa, Ruby!"
lowercase_string = original_string.downcase
puts lowercase_string
# => "kon'nichiwa, ruby!"
```

`downcase!`メソッドもあります。これは元の文字列を直接変更します：

```ruby
greeting = "Hello, World!"
greeting.downcase!
puts greeting
# => "hello, world!"
```

## Deep Dive (詳細情報)
Rubyでは、`downcase`メソッドは長い歴史を持っており、文字列を小文字に変換する簡単で効率的な方法として定着しています。しかし、`downcase`はロケールに依存しないため、例えばトルコ語のように特殊な大文字小文字のルールがある言語は正しく変換されないことがあります。このような場合、Unicode対応の`downcase`の使用が検討されるべきです。

Ruby 2.4からは、`String#downcase`メソッドはUnicodeのケースマッピングを使っており、多くの言語の文字を適切に小文字に変換できるようになりました。しかし、パフォーマンスや互換性の観点から代替手段も考えられるべきです。たとえば、正規表現と`gsub`メソッドを組み合わせることでカスタマイズされた変換を行うことも可能です。

```ruby
# Unicodeを考慮した小文字化
original_string = "Straße" # ドイツ語で "street"
lowercase_string = original_string.downcase
puts lowercase_string
# => "straße"
```

## See Also (参照情報)
- Rubyのドキュメントで`downcase`メソッドの詳細: [Ruby API](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase)
- Unicodeと文字列操作に関する情報: [Unicode.org](http://www.unicode.org/)
- Rubyの他の文字列操作メソッド: [Ruby API String Methods](https://ruby-doc.org/core-3.1.2/String.html)
