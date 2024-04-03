---
date: 2024-01-20 17:51:24.324291-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.832448-06:00'
model: gpt-4-1106-preview
summary: .
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
