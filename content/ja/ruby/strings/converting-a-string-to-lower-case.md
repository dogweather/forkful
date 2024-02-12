---
title:                "文字列を小文字に変換"
aliases: - /ja/ruby/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:11.226599-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-string-to-lower-case.md"
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
