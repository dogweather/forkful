---
date: 2024-01-20 17:43:48.963817-07:00
description: "How to: (\u3084\u308A\u65B9) \u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\
  \u3068\u6587\u5B57\u306E\u524A\u9664\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\
  \u306E\u4E16\u754C\u3067\u306F\u53E4\u304F\u304B\u3089\u884C\u308F\u308C\u3066\u3044\
  \u307E\u3059\u3002\u6B63\u898F\u8868\u73FE (Regular Expressions)\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.622772-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u3068\u6587\
  \u5B57\u306E\u524A\u9664\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u306E\u4E16\
  \u754C\u3067\u306F\u53E4\u304F\u304B\u3089\u884C\u308F\u308C\u3066\u3044\u307E\u3059\
  \u3002\u6B63\u898F\u8868\u73FE (Regular Expressions) \u304C\u666E\u53CA\u3059\u308B\
  \u3068\u3001\u305D\u308C\u306F\u3055\u3089\u306B\u5F37\u529B\u306A\u30C4\u30FC\u30EB\
  \u3068\u306A\u308A\u307E\u3057\u305F\u3002Ruby\u306E`gsub`\u30E1\u30BD\u30C3\u30C9\
  \u306F\u3001\u6587\u5B57\u5217\u4E2D\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u30DE\u30C3\
  \u30C1\u3059\u308B\u90E8\u5206\u3092\u7F6E\u304D\u63DB\u3048\u307E\u305F\u306F\u524A\
  \u9664\u3059\u308B\u305F\u3081\u306B\u5E83\u304F\u4F7F\u308F\u308C\u3066\u3044\u307E\
  \u3059\u3002\u3053\u306E\u30E1\u30BD\u30C3\u30C9\u306F\u3001\u975E\u5E38\u306B\u6C4E\
  \u7528\u7684\u3067\u3042\u308A\u3001\u8907\u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u3067\
  \u3082\u6271\u3046\u3053\u3068\u304C\u53EF\u80FD\u3067\u3059."
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

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
