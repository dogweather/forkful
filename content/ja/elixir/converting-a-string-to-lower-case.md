---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、すべての大文字を対応する小文字に置き換えることです。一貫性を保つために、またはユーザー入力を扱う際に、プログラマーはこの技術をよく使用します。

## 方法：

次はElixirで文字列を小文字に変換する方法です。

```elixir
string = "HELLO, WORLD!"
downcase_string = String.downcase(string)
IO.puts(downcase_string)
```
実行結果は次の通りです。
```elixir
hello, world!
```

## 深堀り

1. 過去の文脈: 古いプログラミング言語では、文字列を小文字に変換するのはより手間がかかりました。しかし、現代の多くの言語では、組み込み関数を提供しています。
2. 代替方法: `downcase`関数の代わりに、各文字に`char_downcase`を適用することも可能ですが、`downcase`関数の方が簡単で効率的です。
3. 実装詳細: Elixirの`downcase`関数は、Unicodeの大文字と小文字変換をサポートしています。これは、様々な言語と文字に対応していることを意味します。

## 関連情報

- Elixirの公式ドキュメンテーションには詳細な説明があります: [https://hexdocs.pm/elixir/String.html#downcase/2](https://hexdocs.pm/elixir/String.html#downcase/2)
- "Programming Elixir"の本も参考になります: [https://pragprog.com/titles/elixir16/programming-elixir-1-6/](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)