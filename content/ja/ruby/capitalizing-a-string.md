---
title:    "Ruby: 文字列の最初を大文字にする"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

ソフトウェア開発において、文字列を大文字に変換することは非常に重要です。これは、文字列を正しく処理するために必要な手法の一つです。

## 方法

文字列を大文字に変換するには、Rubyの `upcase` メソッドを使用します。例えば、以下のように記述します。

```ruby
my_string = "hello world"
puts my_string.upcase
```

このコードを実行すると、"HELLO WORLD"という出力が得られます。

## ディープダイブ

文字列を大文字に変換する背景には、コンピューター内部で文字を保存する際の規則があります。例えば、英語の大文字と小文字を区別する必要があります。そのため、Rubyでは `upcase` メソッドを使用することで、文字列を大文字に変換することができます。

## 参考リンク

- [String#upcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-upcase)
- [Ruby 入門: 文字列の大文字・小文字を切り替えて比較する](https://www.javadrive.jp/ruby/string/index7.html)
- [文字列 manipulation のための Ruby ガイド](https://itnext.io/the-complete-guide-to-string-manipulation-in-ruby-366f7fc6512b)

## また見る

参考になる他のRubyプログラミングの記事を読む際は、次のタグを確認してください。

- [ルビー入門](https://ruby-jp.github.io/ruby-manual/)
- [Rubyプログラミング入門](https://qiita.com/hiiragi/items/8c83936a05e34c3fad1a)