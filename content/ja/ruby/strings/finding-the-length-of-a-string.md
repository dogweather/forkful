---
date: 2024-01-20 17:48:18.547239-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.839006-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## How to: (やり方)
```ruby
str = "こんにちは"
puts str.length        # 文字列の長さを出力
puts str.bytesize      # バイトサイズを出力
puts str.chars.count   # 文字数をカウントして出力
```

Sample output (出力例):
```
5
15
5
```

## Deep Dive (掘り下げ)
Rubyでは文字列の長さを知る方法が簡単に提供されています。`length`や`size`メソッドを使えば、文字列内の文字数が返ります。歴史的には遡ることRuby 1.8まで、マルチバイト文字（例えば日本語）に対応していませんでした。しかし、Ruby 1.9からはエンコーディングを意識した文字列処理が導入されました。`bytesize`はバイト単位の長さであり、エンコーディングによって異なるので注意が必要です。また、`chars`を使って各文字にアクセスし、`count`で数えることもできます。これは文字列を配列のように扱えるためです。

## See Also (関連情報)
- Rubyドキュメンテーションの[String#length](https://docs.ruby-lang.org/ja/latest/method/String/i/length.html)
- Rubyドキュメンテーションの[String#bytesize](https://docs.ruby-lang.org/ja/latest/method/String/i/bytesize.html)
- [Ruby 1.9 Release Notes](https://www.ruby-lang.org/en/news/2007/12/25/ruby-1-9-0-released/) (マルチバイト文字列対応の説明)
