---
title:                "パターンに一致する文字を削除する"
date:                  2024-01-20T17:43:48.963817-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？何のために？)
Rubyでパターンに合う文字を削除するのは、文字列から不要な文字や文字の並びを取り除くことです。プログラマーは、データの整形や文字列のクリーニング、フォーマットの統一のためにこれを行います。

## How to: (やり方)
```Ruby
# gsubメソッドを使う例
str = "Hello, 123 World!"
clean_str = str.gsub(/[0-9]/, '')
puts clean_str  # => "Hello,  World!"

# deleteメソッドを使う例
str = "foobar123"
clean_str = str.delete('0-9')
puts clean_str  # => "foobar"

# 変数を使って削除パターンを指定することもできます
digits_to_remove = "0123456789"
str = "This is 2023!"
clean_str = str.delete(digits_to_remove)
puts clean_str  # => "This is !"
```

## Deep Dive (掘り下げ)
パターンマッチと文字の削除は、テキスト処理の世界では古くから行われています。正規表現 (Regular Expressions) が普及すると、それはさらに強力なツールとなりました。Rubyの`gsub`メソッドは、文字列中のパターンにマッチする部分を置き換えまたは削除するために広く使われています。このメソッドは、非常に汎用的であり、複雑なパターンでも扱うことが可能です。

一方で、`delete`メソッドは、単純な文字範囲や個別の文字を削除する際にはより直接的で、速い方法を提供します。`gsub`よりも高速に動作する場面が多く、文字列内の特定の文字セットを素早く取り除く場合に便利です。

どちらを使うかは、目的とする文字列の処理によって異なります。`gsub`で複雑な検索置換を行い、`delete`で簡単な文字削除を行うのが一般的です。

## See Also (関連情報)
- Ruby の正規表現にについて: [Ruby 正規表現ドキュメント](https://docs.ruby-lang.org/en/2.6.0/Regexp.html)
- `String#gsub` メソッド: [Ruby API ドキュメント](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- `String#delete` メソッド: [Ruby API ドキュメント](https://ruby-doc.org/core-2.7.0/String.html#method-i-delete)
- 文字列操作のベストプラクティス: [Ruby スタイルガイド](https://rubystyle.guide/#string-interpolation)
