---
title:                "文字列の補間"
aliases:
- /ja/ruby/interpolating-a-string/
date:                  2024-01-20T17:51:24.324291-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列展開とは、文字列内に変数や式の値を挿入することです。これを行う理由は、動的に変化するデータを扱ったり、読みやすい文字列を作るためです。

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
