---
date: 2024-01-20 17:39:11.226599-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3068\u306F\
  \u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u4E2D\u306E\u6587\u5B57\u5217\u3092\u5168\u3066\
  \u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u51E6\
  \u7406\u306F\u3001\u691C\u7D22\u3084\u30BD\u30FC\u30C8\u6642\u306B\u5927\u6587\u5B57\
  \u5C0F\u6587\u5B57\u306E\u9055\u3044\u3092\u7121\u8996\u3059\u308B\u76EE\u7684\u3084\
  \u3001\u7D71\u4E00\u3055\u308C\u305F\u30C7\u30FC\u30BF\u5F62\u5F0F\u3092\u4FDD\u3064\
  \u305F\u3081\u306B\u3088\u304F\u884C\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.833790-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3068\u306F\
  \u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u4E2D\u306E\u6587\u5B57\u5217\u3092\u5168\u3066\
  \u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u51E6\
  \u7406\u306F\u3001\u691C\u7D22\u3084\u30BD\u30FC\u30C8\u6642\u306B\u5927\u6587\u5B57\
  \u5C0F\u6587\u5B57\u306E\u9055\u3044\u3092\u7121\u8996\u3059\u308B\u76EE\u7684\u3084\
  \u3001\u7D71\u4E00\u3055\u308C\u305F\u30C7\u30FC\u30BF\u5F62\u5F0F\u3092\u4FDD\u3064\
  \u305F\u3081\u306B\u3088\u304F\u884C\u308F\u308C\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
文字列を小文字に変換とは、プログラム中の文字列を全て小文字にすることです。この処理は、検索やソート時に大文字小文字の違いを無視する目的や、統一されたデータ形式を保つためによく行われます。

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
