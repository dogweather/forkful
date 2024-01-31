---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
大文字化とは、文字列の先頭を大文字にすることです。一貫性を持たせたり、タイトルや固有名詞を適切に表示するためにプログラマーはこれを行います。

## How to (やり方)
```ruby
# capitalizeメソッドを使う
phrase = "ruby programming"
puts phrase.capitalize
# 出力: "Ruby programming"

# 文字列の各単語を大文字化するには
puts phrase.split.map(&:capitalize).join(' ')
# 出力: "Ruby Programming"
```

## Deep Dive (詳細な解説)
文字列を大文字化する方法は、Rubyが登場した1995年から存在します。`.capitalize` は単純で便利ですが、他にも方法があります。例えば、「Active Support」の `titleize` はRailsで使用でき、より複雑なケースに対応します。実装の内部では、`capitalize` は最初の文字のASCIIコードを使って変換を行います。

## See Also (関連情報)
- Railsガイド：[Active Support Core Extensions](https://guides.rubyonrails.org/active_support_core_extensions.html)
- ASCIIコード表：[ASCII codes](https://www.asciitable.com/)
