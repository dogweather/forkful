---
title:                "サブストリングの抽出"
html_title:           "Ruby: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を抽出することは、プログラマーが文字列を解析するために行う方法です。これにより、特定の部分文字列を取得したり、文字列内の特定のパターンをマッチングしたりすることができます。抽出されたデータを使用してプログラムをより効率的に動作させることができるため、プログラマーはこの手法を使用します。

## 方法：

以下のような方法で、文字列から部分文字列を抽出することができます。

```Ruby
# 文字列を定義
text = "今日はいい天気です"

# 4文字目から6文字目までの部分文字列を抽出
puts text[3..5]
# => "いい"

# 文字列の長さを使用して、最後の文字を抽出
puts text[text.length-1]
# => "す"

# 正規表現を使用して、"天気"を含む部分文字列を抽出
puts text[/天気/]
# => "天気"
```

## 深堀り：

文字列から部分文字列を抽出する方法は、プログラミングの歴史の中で常に重要な役割を果たしてきました。以前は、マニュアルに書かれた固定の方法が使用されていましたが、正規表現の出現により、より柔軟な抽出方法が可能になりました。また、データベースやAPIからの文字列を抽出することにも使用されています。

## 関連情報：

- [Rubyの正規表現ガイド](https://www.javadrive.jp/ruby/regex/index1.html)
- [Rubyの文字列操作について](https://railsguides.jp/active_support_core_extensions.html#tagged-a-in-memoize)
- [文字列を抽出する方法についての記事](https://medium.com/rubycademy/reverse-a-string-in-ruby-239d8006c9e3)