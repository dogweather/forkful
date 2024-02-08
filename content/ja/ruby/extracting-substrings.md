---
title:                "部分文字列の抽出"
aliases:
- ja/ruby/extracting-substrings.md
date:                  2024-01-20T17:46:31.677926-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

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
