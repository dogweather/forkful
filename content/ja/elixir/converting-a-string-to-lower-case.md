---
title:                "文字列を小文字に変換する"
html_title:           "Elixir: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

ここでは、文字列を小文字に変換する理由について説明します。文字列を小文字に変換することは、検索や比較を行う際に、文字の大文字・小文字を区別しないようにするためです。

## How To

文字列を小文字に変換するには、Elixirの`String.downcase/1`関数を使用します。以下は、例となるコードとその出力です。

```Elixir
string = "Hello World"
lowercase_string = String.downcase(string)
IO.puts lowercase_string
```

出力結果：
```
hello world
```

## Deep Dive

Elixirでは、文字列はimmutable（変更不可）なので、元の文字列を変更するのではなく、新しい文字列を返します。また、`String.downcase/1`関数はUnicodeに対応しており、国際化された文字にも適用することができます。

## See Also

- [Elixir公式ドキュメント - String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/2)
- [Unicodeと文字列操作についての詳細な説明 (英語)](https://www.stubbornella.org/content/2010/06/25/the-incredible-shrinking-string/)