---
date: 2024-01-20 17:46:31.677926-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u3092\u629C\
  \u304D\u51FA\u3059\u3053\u3068\u3092\u3001\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\
  \u306E\u62BD\u51FA\u3068\u8A00\u3044\u307E\u3059\u3002\u30C7\u30FC\u30BF\u51E6\u7406\
  \u3084\u691C\u7D22\u3067\u6B63\u78BA\u306A\u60C5\u5831\u3092\u5F97\u308B\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.835836-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u3092\u629C\
  \u304D\u51FA\u3059\u3053\u3068\u3092\u3001\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\
  \u306E\u62BD\u51FA\u3068\u8A00\u3044\u307E\u3059\u3002\u30C7\u30FC\u30BF\u51E6\u7406\
  \u3084\u691C\u7D22\u3067\u6B63\u78BA\u306A\u60C5\u5831\u3092\u5F97\u308B\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## What & Why? (何となぜ？)
文字列から特定の部分を抜き出すことを、サブストリングの抽出と言います。データ処理や検索で正確な情報を得るために行います。

## How to: (やり方)
```Ruby
# 与えられた文字列からサブストリングを取り出す基本的な例
str = "こんにちは、Rubyの世界へ！"
# 文字位置を指定して取り出す (位置5から始まる3文字)
substring = str[5, 3]
puts substring  # => "は、R"

# 文字列の範囲を使って取り出す (位置7から位置12まで)
substring_range = str[7..12]
puts substring_range  # => "Rubyの"

# 正規表現を使って取り出す
regex_substring = str[/[あ-ん]+/]
puts regex_substring  # => "こんにちは"
```

## Deep Dive (詳細情報)
サブストリングの抽出はRubyが登場した1995年から存在しています。`slice`メソッドや`[]`オペレータを使って文字列から部分文字列を取り出せます。これらは内部的に同じ機構を使用しています。正規表現は強力なパターンマッチングを提供し、複雑なパターンのサブストリングも抽出できます。`slice`メソッドは`slice!`バージョンもあり、これを使用すると元の文字列からサブストリングを削除できます。また、範囲オブジェクトを使って複数の文字を取り出す方法も便利で、より直感的な操作が可能です。

## See Also (参照)
- Rubyの公式ドキュメントの `String` クラス: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- Ruby正規表現ガイド: [http://www.ruby-doc.org/core-2.7.1/Regexp.html](http://www.ruby-doc.org/core-2.7.1/Regexp.html)
