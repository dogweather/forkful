---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ?

部分文字列を抽出するとは、文字列の一部を取り出すことを意味します。プログラマーがこれを行う主な理由は、大きなデータから特定の情報を検索し、それを利用または解析するためです。

## 方法:

Elixirでは、多くの方法で部分文字列を抽出することができます。以下になんのちゃんがElixirで部分文字列を抽出するための一例を示します。

```elixir
String.slice("プログラミング", 0..4)
```

出力:

```elixir
"プログラ"
```

この例では、文字列"プログラミング"からインデックス0から4の部分文字列を抽出しています。

## 深堀り

部分文字列を抽出する方法は古くから存在しており、使われてきた多くのプログラミング言語にその概念があります。Elixirでは、その操作は `String.slice/2` 関数を利用して行います。

代わりに `binary_part/3` 関数を用いることもでき、これはバイト単位で部分文字列を抽出します。こちらはエンコーディングに関係なく動作しますが、文字列操作では`String.slice/2`の使用が推奨されています。

`String.slice/2`の実装では、Elixirはビーム(エーリングのVM)上での文字列表現を利用し、部分文字列の効率的な抽出を可能にしています。

## 参照

部分文字列の抽出以外にもElixirは多くの文字列操作をサポートしています。詳細はElixir公式ドキュメントの[String module](https://hexdocs.pm/elixir/String.html)をご覧ください。

また、部分文字列の抽出と類似の操作である文字列の分割について知りたい方は、[String.split/1](https://hexdocs.pm/elixir/String.html#split/1)をご覧ください。