---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列補間（String Interpolation）とは、文字列内に変数を直接埋め込む機能を意味します。これにより、プログラマーは効率的にダイナミックなコードを書くことができます。

## やり方：

Elixirでは、"#{}" を使用して文字列補間を行います。以下に簡単な例を示します。

```Elixir
name = "Tom"
IO.puts("Hello, #{name}")
```

上記のコードを実行すると、出力は次のようになります。

```Elixir
"Hello, Tom"
```

## ディープダイブ：

歴史的な文脈としては、文字列補間は多くのプログラミング言語で使用されています。Elixirでは、これをシンプルで直感的な構文で実現しています。

文字列補間の代替手段としては、文字列連結があります。しかし、文字列補間はよりシンプルで美しいコードを生成できるのが特徴です。

Elixirでは、文字列補間は"^"を用いて実行時までその評価を遅延させることができます。このため、大量のデータ操作でパフォーマンスを向上させることが可能です。

## 参考リンク：

- Elixir公式ドキュメント「Strings」
  (https://hexdocs.pm/elixir/String.html)
- Elixir School「Strings」
  (https://elixirschool.com/ja/lessons/basics/strings/)