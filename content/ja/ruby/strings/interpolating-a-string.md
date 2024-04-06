---
date: 2024-01-20 17:51:24.324291-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:55.007912-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u5C55\u958B\u306FRuby\u521D\u671F\u304B\
  \u3089\u3042\u308B\u6A5F\u80FD\u3067\u3059\u3002`#{}`\u3092\u4F7F\u3046\u3068\u3001\
  \u305D\u306E\u4E2D\u306E\u30B3\u30FC\u30C9\u304C\u8A55\u4FA1\u3055\u308C\u3001\u6587\
  \u5B57\u5217\u306B\u5909\u63DB\u3055\u308C\u307E\u3059\u3002`+`\u3092\u4F7F\u3063\
  \u3066\u6587\u5B57\u5217\u3092\u7D50\u5408\u3059\u308B\u65B9\u6CD5\u3082\u3042\u308A\
  \u307E\u3059\u304C\u3001\u6587\u5B57\u5217\u5C55\u958B\u306E\u65B9\u304C\u9AD8\u901F\
  \u3067\u3001\u30B3\u30FC\u30C9\u3082\u304D\u308C\u3044\u306B\u306A\u308A\u307E\u3059\
  \u3002\u5185\u90E8\u7684\u306B\u306F\u3001Ruby\u306E\u30A4\u30F3\u30BF\u30FC\u30D7\
  \u30EA\u30BF\u304C`#{}`\u306E\u5185\u5BB9\u3092\u8A55\u4FA1\u3057\u3001\u7D50\u679C\
  \u3092\u5143\u306E\u6587\u5B57\u5217\u306B\u57CB\u3081\u8FBC\u307F\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## How to: (方法)
```Ruby
name = "太郎"
age = 28

# 文字列展開を使って変数を埋め込む
greeting = "こんにちは、#{name}さん。あなたは#{age}歳ですね。"

puts greeting
# 出力: こんにちは、太郎さん。あなたは28歳ですね。
```

## Deep Dive (深い掘り下げ)
文字列展開はRuby初期からある機能です。`#{}`を使うと、その中のコードが評価され、文字列に変換されます。`+`を使って文字列を結合する方法もありますが、文字列展開の方が高速で、コードもきれいになります。内部的には、Rubyのインタープリタが`#{}`の内容を評価し、結果を元の文字列に埋め込みます。

## See Also (関連情報)
- Rubyの公式ドキュメントの文字列展開のセクション: [Ruby String Interpolation](https://docs.ruby-lang.org/en/trunk/syntax/literals_rdoc.html#label-Strings)
