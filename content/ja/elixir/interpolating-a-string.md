---
title:                "文字列の補間"
html_title:           "Elixir: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何そしてなぜ？
文字列を插入することは、変数や式を文字列に置き換えることです。これは、プログラマーが使用することで、より動的な文字列を作成することができます。

プログラマーは、データベースクエリ、ログメッセージの作成、テンプレートなど、さまざまな場面で文字列の挿入を使用します。これにより、より柔軟で効率的なコードが書けます。

## 使い方：
```Elixir
name = "John"
age = 25
"Elixir プログラミングを始めましょう #{name}. 年齢は #{age} 歳です。"
```
このコードでは、変数の値を文字列の中に挿入しています。出力は次のようになります:

 ```Elixir
"Elixir プログラミングを始めましょう John. 年齢は 25 歳です。"
```

また、式を文字列の中に挿入することもできます。例えば:

```Elixir
n = 5
"5を足すと#{n + 5}になります。"
```

このコードでは、変数の値を使って式を作成し、文字列の中に挿入しています。出力は次のようになります:

```Elixir
"5を足すと10になります。"
```

## 詳細：
文字列の挿入は、ドイツのプログラマーであるミトルヘアト・シュトラト(中略)によって発明されました。多くのプログラミング言語で使用されていますが、Elixirではより簡潔で柔軟な構文を提供しています。

他のオルタナティブとしては、文字列連結が挙げられますが、これは複数の文字列を組み合わせる必要があり、コードが長くなりがちです。

Elixirでは、文字列の挿入は「#{}」で囲むことで実現されています。この構文は、Elixir内部で「Kernel.SpecialForms.interpolation/2」と呼ばれるマクロによって処理されます。

## 関連リンク：
- [Elixir公式ドキュメント](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#interpolation/2)
- [ミトルヘアト・シュトラトによる文字列挿入の記事](https://blog.8thlight.com/mohit-srate/2014/02/17/string-interpolation-in-programming-languages.html)
- [Rubyでの文字列挿入](https://ruby-doc.org/core-2.5.0/doc/syntax/literals_rdoc.html#label-String+Interpolation)