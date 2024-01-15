---
title:                "テキストの検索と置換"
html_title:           "Elixir: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ
文章の検索と置換を行う理由は、特定のキーワードを含む文を一括で変更したい場合や、誤ったスペルを修正するためなど、大量の文書を効率的に編集するためです。

## やり方
Elixirでテキストの検索と置換を行うには、`String.replace/3`関数を使用します。例えば、以下のコードで"Hello"を"こんにちは"に置換することができます。

```
Elixir
string = "Hello World"
new_string = String.replace(string, "Hello", "こんにちは")
IO.puts new_string
```

出力結果は、"こんにちは World"となります。

## ディープダイブ
Elixirでは`String.replace/3`の他にも、正規表現を使用した検索と置換ができる`Regex.replace/3`があります。この関数を使用すると、より柔軟な置換が可能になります。また、文字列だけでなくリストやタプルにも置換を行うことができます。

## 参考リンク
- Elixirの公式ドキュメント: https://hexdocs.pm/elixir/String.html#replace/3
- 正規表現の基本: https://qiita.com/mogulla3/items/a56a0d08a2a16c71f651
- 文字列操作のチートシート: https://devhints.io/elixir-string