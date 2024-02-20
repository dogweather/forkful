---
date: 2024-01-20 17:48:18.547239-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u8ABF\u3079\u308B\u3068\u306F\
  \u3001\u6587\u5B57\u304C\u3044\u304F\u3064\u4E26\u3093\u3067\u3044\u308B\u304B\u6570\
  \u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u30C7\u30FC\u30BF\u3092\u691C\u8A3C\u3057\u305F\u308A\u3001\u7279\u5B9A\u306E\u51E6\
  \u7406\u3092\u5236\u5FA1\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.944641
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u8ABF\u3079\u308B\u3068\u306F\
  \u3001\u6587\u5B57\u304C\u3044\u304F\u3064\u4E26\u3093\u3067\u3044\u308B\u304B\u6570\
  \u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u30C7\u30FC\u30BF\u3092\u691C\u8A3C\u3057\u305F\u308A\u3001\u7279\u5B9A\u306E\u51E6\
  \u7406\u3092\u5236\u5FA1\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを調べるとは、文字がいくつ並んでいるか数えることです。プログラマーはデータを検証したり、特定の処理を制御したりするためにこれを行います。

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
