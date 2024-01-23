---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:48:18.547239-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/finding-the-length-of-a-string.md"
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
